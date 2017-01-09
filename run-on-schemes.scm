#!/usr/bin/chibi-scheme -r
;; set -x ANSI_ESCAPES_ENABLED 1
(import (scheme base)
        (scheme time)
        (chibi)
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

;;;; Known Scheme implementations

(define-scheme 'bigloo '("bigloo")
(lambda (bigloo input-filename)
    (let-values (((output error status time) (execute-runner (string-append bigloo " " input-filename " -O6 -call/cc -copt -O3 -o /tmp/bigloo"))))
      (values output error status time "/tmp/bigloo")))
  (lambda (bigloo input-file executable)
    (execute-runner executable))
  (make-version-finder #f "Bigloo (" #\)))
(define-scheme 'biwascheme '("biwas")
  #f
  #f
  (make-version-finder "--version" " version " #f))
(define-scheme 'bones '("bones")
  (lambda (bones input-file)
    (let-values (((bones-output bones-error bones-exit-status bones-time)
                  (execute-runner (show #f bones " " input-file " -o /tmp/bones.s"))))
      (if (zero? bones-exit-status)
          (let-values (((nasm-output nasm-error nasm-exit-status nasm-time)
                        (execute-runner (show #f "nasm -I/usr/share/bones/ -f elf64 /tmp/bones.s -o /tmp/bones.o"))))
            (if (zero? nasm-exit-status)
                (let-values (((gcc-output gcc-error gcc-exit-status gcc-time)
                              (execute-runner (show #f "gcc /tmp/bones.o -o /tmp/bones"))))
                  (values (string-append bones-output nasm-output gcc-output)
                          (string-append bones-error nasm-output gcc-output)
                          gcc-exit-status
                          (+ bones-time nasm-time gcc-time)
                          "/tmp/bones"))
                (values (string-append bones-output nasm-output)
                        (string-append bones-error nasm-error)
                        nasm-exit-status
                        (+ bones-time nasm-time)
                        "/tmp/bones")))
          (values bones-output bones-error bones-exit-status bones-time "/tmp/bones"))))
  (lambda (bones-path input-file executable)
    (execute-runner executable))
  (make-version-finder "-v" "" #f))
(define-scheme 'chez '("chez-scheme")
  #f
  (simple-parameter-runner "-q")
  (lambda (executable scheme)
    ;; read version from stderr
    (string-trim (second (process->output+error+status (string-append executable " --version"))) #\newline)))
(define-scheme 'chibi '("chibi-scheme")
  #f
  #f
  (make-version-finder "-V" "chibi-scheme " #\space))
(define-scheme 'chicken '("csc")
  (lambda (executable input-filename)
    (let-values (((output error status time) (execute-runner (string-append executable " -o /tmp/chicken " input-filename))))
      (values output error status time "/tmp/chicken")))
  (lambda (bones-path input-file executable)
    (execute-runner executable))
  (make-version-finder "-version" "Version " #\space))
(define-scheme 'chicken-csi '("csi")
  #f
  (simple-parameter-runner "-q")
  (make-version-finder "-version" "Version " #\space))
(define-scheme 'cyclone '("cyclone")
  #f
  #f
  (make-version-finder "-v" "Version " #\space))
;; (define-scheme 'foment '("foment")
;;   #f
;;   #f
;;   #f)
(define-scheme 'gambitc '("gambitc")
  (lambda (gambitc input-file)
    (let-values (((output error status time) (execute-runner (show #f gambitc " -o /tmp/gambitc -exe " input-file))))
      (values output error status time "/tmp/gambitc")))
  (lambda (gambitc input-file executable)
    (execute-runner executable))
  (make-version-finder "-v" "" #\space))
(define-scheme 'gauche '("gosh")
  #f
  #f
  (make-version-finder "-V" ", version " #\space))
(define-scheme 'guile '("guile")
  #f
  #f
  (make-version-finder "--version" "guile (GNU Guile) " #\newline))
(define-scheme 'ironscheme '("ironscheme")
  #f
  #f
  (make-version-finder "-V" "" #\newline))
(define-scheme 'kawa '("kawa")
  #f
  #f
  (make-version-finder "--version" "Kawa " #\newline))
(define-scheme 'larceny '("larceny")
  #f
  (simple-parameter-runner "--r7rs --program")
  (make-version-finder "--version" "Larceny " #\space))
(define-scheme 'mit '("mit-scheme")
  #f
  (simple-parameter-runner "--quiet --no-init-file --load")
  (make-version-finder "--version" "  Release " #\space))
(define-scheme 'mosh '("mosh-scheme")
  #f
  #f
  #f) ;; outputs to stderr
(define-scheme 'owl-lisp '("ol")
  #f
  #f
  (make-version-finder "--version" "Owl Lisp " #\newline))
;; (define-scheme 'petit-larceny '("larceny")
;;   #f
;;   (simple-parameter-runner "--r7rs --program")
;;   (make-version-finder "--version" "Larceny " #\space))
(define-scheme 'petite-chez '("petite")
  #f
  (simple-parameter-runner "-q")
  (lambda (executable scheme)
    (string-trim (second (process->output+error+status (string-append executable " --version"))) #\newline)))
(define-scheme 'racket '("racket")
  #f
  #f
  (lambda x (let ((str (apply (make-version-finder "--version" "Welcome to Racket v" #\newline) x)))
         ;; strip end-of-sentence #\.
         (substring str 0 (- (string-length str) 1)))))
(define-scheme 'rscheme '("fshell")
  #f
  (simple-parameter-runner "-q")
  (make-version-finder "--version" "" #\newline))
;; (define-scheme 'rscheme-rsc '("rsc") #f) ;; compiling
;; (define-scheme 'rhizome '("pisc") #f) ;; needs multiple executables ;-/
(define-scheme 'sagittarius '("sagittarius")
  #f
  #f
  (make-version-finder "--version" ", version " #\space))
;; (define-scheme 'scheme48 '("scheme48") #f) ;; todo: fix problems with loading script files
(define-scheme 'stalin-chicken '("chicken-stalin")
  (lambda (executable input-filename)
    ;; TODO: run through alexpander first
    (let-values (((output error exit-status time)
                  (execute-runner (string-append executable " -On -Ob -Om -Or -Ot -d -d1 -k -copt -O3 " input-filename))))
      (values output error exit-status time (substring input-filename 0 (- (string-length input-filename) 4)))))
  (lambda (stalin-path input-file executable)
    (execute-runner executable))
  (lambda (executable scheme)
    "unknown"))
(define-scheme 'tinyscheme '("tinyscheme")
  #f
  #f
  ;; tinyscheme prints to stdout, so run with no parameters, no stdin will immediately terminate it after printing the version
  (make-version-finder #f "TinyScheme " #f))
(define-scheme 'vicare '("vicare")
  #f
  #f
  (make-version-finder "--version" " version " #\,))
(define-scheme 'ypsilon '("ypsilon")
  #f
  #f
  (make-version-finder "--version" "Ypsilon " #\space))



(define (find-schemes)
  "Return a list of (<executable-path> . <scheme record>). Path is #f if not found."
  (map (lambda (scheme)
         (let ((executable (find (lambda (x) x) (map find-executable (scheme-executable-names scheme)))))
           (cons executable scheme)))
       (hash-table-values *schemes*)))

(define (main arguments)
  (run-application
   `(run-on-schemes
     "Run-on-Schemes"
     (or
      (list "List available Scheme implementations (-v for details)" (@ (verbose boolean (#\v))) (,list-schemes))
      (run "Run an input with multiple Scheme implementations."
           (@ (scheme string (#\i) "Include only matching Schemes, comma-separated list")
              (text boolean (#\t) "Output text, not s-expressions")
              (no-color boolean (#\c) "Disable color output"))
           (,run-schemes))
      (help "Print this help." (,app-help-command))))
   (cdr (command-line)))
  -1)

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
         (matching-schemes-raw (conf-get config '(command run scheme)))
         (matching-schemes (if matching-schemes-raw (map string-trim (string-split matching-schemes-raw #\,)) #f)))
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
