;; set -x ANSI_ESCAPES_ENABLED 1
(import (scheme base)
        (scheme time)
        (chibi)
        (chibi io)
        (chibi pathname) ;; path-directory
        (chibi filesystem) ;; file-exists? file-is-executable?
        (srfi 9) ;; define-record-type
        (chibi string) ;; string-suffix? string-split
        (srfi 98) ;; get-environment-variable
        (srfi 69) ;; hash tables
        (srfi 1) ;; any
        (chibi show)
        (chibi process)
        (chibi term ansi)
        (chibi app)
        (chibi config))

;; Path to this script file.
(define (script-path)
  (let* ((pid (number->string (current-process-id)))
         (command (string-append "ps -eo command " pid))
         (standard-output (car (process->output+error+status command)))
         (lines (string-split standard-output #\newline))
         (second-line (cadr lines)))
    (cadr (string-split second-line))))

;; Relative to this script, the path to the default schemes definition file.
(define (default-schemes-path)
  (string-append (path-directory (script-path)) "/default-schemes.scm"))

(define (find-executable name)
  (let ((paths (string-split (get-environment-variable "PATH") #\:)))
    (let loop ((paths paths))
      (if (null? paths)
          #f
          (let* ((path (car paths))
                 (full-path (string-append path (if (string-suffix? "/" path) "" "/") name)))
            (if (and (file-exists? full-path)
                     (file-is-executable? full-path))
                full-path
                (loop (cdr paths))))))))

(define *schemes* (make-hash-table))

(define-record-type scheme
  (make-scheme name executable-names make-compile make-run get-version)
  scheme?
  (name scheme-name)
  (executable-names scheme-executable-names)
  (make-compile scheme-make-compile)
  (make-run scheme-make-run)
  (get-version scheme-get-version))

(define (define-scheme name executable-names make-compile make-run get-version)
  (hash-table-set! *schemes* name (make-scheme name executable-names make-compile make-run get-version)))

(define (make-version-finder param line eol)
  (lambda (executable scheme)
    (let* ((lines (process->string-list (if param (string-append executable " " param) executable)))
           (prefix line)
           (release (find (lambda (string)
                            (string-contains string prefix))
                          lines))
           (offset (if release (+ (string-cursor->index release (string-contains release prefix)) (string-length prefix)) #f))
           (version (if release (substring release offset (if eol (string-find release eol (string-index->cursor release offset)) (string-length release))) "unknown")))
      version)))

(define (execute-runner command)
  (let* ((before (current-jiffy))
         (res (process->output+error+status command))
         (after (current-jiffy))
         (output (first res))
         (error (second res))
         (exit-status (third res))
         (time (inexact (/ (- after before) (jiffies-per-second)))))
    (values output error exit-status time)))

(define (run-default path input-file)
  (execute-runner (show #f path " " input-file)))

(define (simple-parameter-runner parameters)
  (lambda (executable input-filename compile-data)
    (execute-runner (show #f executable " " parameters " " input-filename))))

(define (find-schemes)
  "Return a list of (<executable-path> . <scheme record>). Path is #f if not found."
  (map (lambda (scheme)
         (let ((executable (find (lambda (x) x) (map find-executable (scheme-executable-names scheme)))))
           (cons executable scheme)))
       (hash-table-values *schemes*)))

(define (list-schemes config spec . arguments)
  (let ((schemes (find-schemes))
        (verbose (conf-get config '(command list verbose))))
    (for-each (lambda (scheme)
                (let ((path (car scheme))
                      (scheme (cdr scheme)))
                  (when (or path verbose)
                    (show #t (magenta (symbol->string (scheme-name scheme))) (space-to 30) " -> " (if path (green path) (red "not found")) nl))))
              schemes)))

(define (run-schemes config spec . arguments)
  (let* ((sexp? (not (conf-get config '(command run text))))
         (no-color? (conf-get config '(command run no-color)))
         (default-schemes (conf-get config '(command run default-schemes)))
         (matching-schemes-raw (conf-get config '(command run scheme)))
         (matching-schemes (if matching-schemes-raw (map string-trim (string-split matching-schemes-raw #\,)) #f)))

    ; Allow for a path to a .scm file be passed, containing a list of schemes to use.
    (if (equal? default-schemes #f)
      (load (default-schemes-path))
      (load default-schemes))

    (parameterize ((ansi-escapes-enabled? (if no-color? #f (ansi-escapes-enabled?))))
      (if (null? arguments)
          (begin
            (show #t "ERROR: No files to run given.")
            -1)
          (let ((input-files arguments))
            (when sexp?
              (show #t "("))
            (for-each (lambda (input-file)
                        (let ((schemes (find-schemes)))
                          (for-each (lambda (scheme)
                                      (let* ((path (car scheme))
                                             (scheme (cdr scheme)))
                                        (when (and path
                                                   (or (not matching-schemes)
                                                       (any (lambda (s)
                                                              (string-contains (symbol->string (scheme-name scheme)) s))
                                                            matching-schemes)))
                                          (let* ((get-version (scheme-get-version scheme))
                                                 (version (if get-version (get-version path scheme) "unknown"))
                                                 (make-compile (scheme-make-compile scheme))
                                                 (make-run (scheme-make-run scheme))
                                                 ;;(compile-command-line (if make-compile (make-compile path input-file) #f))
                                                 ;;(run-command-line (if make-run (make-run path input-file) (show #f path " " input-file)))
                                                 )
                                            (let*-values (((compile-output compile-error compile-exit-status compile-time compile-data-for-run)
                                                           (if make-compile
                                                               (make-compile path input-file)
                                                               (values #f #f #f #f #f)))
                                                          ((output error exit-status time)
                                                           (if (or (not compile-exit-status)
                                                                   (and compile-exit-status (zero? compile-exit-status)))
                                                               (if make-run
                                                                   (make-run path input-file compile-data-for-run)
                                                                   (run-default path input-file))
                                                               (values #f #f #f #f))))
                                              (if sexp?
                                                  (show #t "(" (magenta (symbol->string (scheme-name scheme))) nl)
                                                  (show #t (magenta (symbol->string (scheme-name scheme))) nl))
                                              (let* ()
                                                (define (show-item name-color name data-color data)
                                                  (show #t "  (" (if name-color (name-color name) name) " ")
                                                  (when data-color (display (data-color)))
                                                  (write data)
                                                  (when data-color (display (white-escape)))
                                                  (show #t ")" nl))
                                                (if sexp?
                                                    (begin
                                                      (when compile-time
                                                        (show-item yellow "compile-stdout" #f compile-output)
                                                        (show-item yellow "compile-stderr" #f compile-error)
                                                        (show-item yellow "compile-time" #f compile-time) ;; (inexact (/ (- after before) (jiffies-per-second)))
                                                        (show-item yellow "compile-exit-status" (if (and compile-exit-status (zero? compile-exit-status)) green-escape red-escape) compile-exit-status))
                                                      (show-item yellow "source" #f input-file)
                                                      (show-item yellow "version" #f version)
                                                      (show-item yellow "stdout" #f output)
                                                      (show-item yellow "stderr" #f error)
                                                      (show-item yellow "time" #f time) ;; (inexact (/ (- after before) (jiffies-per-second)))
                                                      (show-item yellow "exit-status" (if (and exit-status (zero? exit-status)) green-escape red-escape) exit-status)
                                                      (show #t ")" nl))
                                                    (if (zero? exit-status)
                                                        (show #t output nl " " (scheme-name scheme) (green " -> OK") nl)
                                                        (show #t error nl " " (scheme-name scheme) (red " -> ERROR") nl)))))))))
                                    schemes)))
                      input-files)
            (when sexp?
              (show #t ")" nl)))))))

(define (main)
  (run-application
   `(run-on-schemes
     "run-on-Schemes"
     (or
      (list "List available Scheme implementations (-v for details)" (@ (verbose boolean (#\v))) (,list-schemes))
      (run "Run an input with multiple Scheme implementations."
           (@ (default-schemes string (#\f) "Path to a file containing a list of scheme definitions to use")
              (scheme string (#\i) "Include only matching Schemes, comma-separated list")
              (text boolean (#\t) "Output text, not s-expressions")
              (no-color boolean (#\c) "Disable color output"))
           (,run-schemes))
      (help "Print this help." (,app-help-command)))))
  -1)

(main)
