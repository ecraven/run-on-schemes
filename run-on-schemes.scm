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
  (make-scheme name executable-names command-line-maker get-version)
  scheme?
  (name scheme-name)
  (executable-names scheme-executable-names)
  (command-line-maker scheme-command-line-maker)
  (get-version scheme-get-version))

(define (define-scheme name executable-names command-line-maker get-version)
  (hash-table-set! *schemes* name (make-scheme name executable-names command-line-maker get-version)))

(define (make-version-finder param line eol)
  (lambda (executable scheme)
    (let* ((lines (process->string-list (string-append executable " " param)))
           (prefix line)
           (release (find (lambda (string)
                            (string-contains string prefix))
                          lines))
           (offset (if release (+ (string-cursor->index release (string-contains release prefix)) (string-length prefix)) #f))
           (version (if release (substring release offset (string-find release eol (string-index->cursor release offset))) "unknown")))
      version)))

;;;; Known Scheme implementations
;; (define-scheme 'bigloo ...) ;; compiling
(define-scheme 'biwascheme '("biwas")
  #f
  (make-version-finder "--version" " version " #\newline))
;; (define-scheme 'bones ...) ;; compiling
(define-scheme 'chez '("chez-scheme")
  (lambda (executable input-filename) (string-append executable " -q " input-filename))
  (lambda (executable scheme)
    (string-trim (second (process->output+error+status (string-append executable " --version"))) #\newline)))
(define-scheme 'chibi '("chibi-scheme")
  #f
  (make-version-finder "-V" "chibi-scheme " #\space))
;; (define-scheme 'chicken '("csc") (lambda (executable input-filename) (string-append executable " -o /tmp/foo " input-filename " ; /tmp/foo"))) ;; compiling
(define-scheme 'chicken-csi '("csi")
  (lambda (executable input-filename) (string-append executable " -q " input-filename))
  (make-version-finder "-version" "Version " #\space))
(define-scheme 'cyclone '("cyclone")
  #f
  (make-version-finder "-v" "Version " #\space))
(define-scheme 'foment '("foment") #f #f)
;; (define-scheme 'gambitc ...) ;; compiling
(define-scheme 'gauche '("gosh")
  #f
  (make-version-finder "-V" ", version " #\space))
(define-scheme 'guile '("guile")
  #f
  (make-version-finder "--version" "guile (GNU Guile) " #\newline)
  )
(define-scheme 'ironscheme '("ironscheme")
  #f
  (make-version-finder "-V" "" #\newline))
(define-scheme 'kawa '("kawa") #f
  (make-version-finder "--version" "Kawa " #\newline))
;; (define-scheme 'larceny '("larceny") #f) ;; todo: fix problems with loading script files
(define-scheme 'mit '("mit-scheme")
  (lambda (executable input-filename) (string-append executable " --quiet --no-init-file --load " input-filename))
  (make-version-finder "--version" "  Release " #\space))
(define-scheme 'mosh '("mosh-scheme")
  #f
  #f) ;; outputs to stderr
(define-scheme 'owl-lisp '("ol")
  #f
  (make-version-finder "--version" "Owl Lisp " #\newline))
;; (define-scheme 'petit-larceny '("larceny") #f) ;; todo
(define-scheme 'petite-chez '("petite")
  (lambda (executable input-filename) (string-append executable " -q " input-filename))
  (lambda (executable scheme)
    (string-trim (second (process->output+error+status (string-append executable " --version"))) #\newline)))
(define-scheme 'racket '("racket")
  #f
  (lambda x (let ((str (apply (make-version-finder "--version" "Welcome to Racket v" #\newline) x)))
         ;; strip end-of-sentence #\.
         (substring str 0 (- (string-length str) 1)))))
(define-scheme 'rscheme '("fshell")
  (lambda (executable input-filename) (string-append executable " -q " input-filename))
  (make-version-finder "--version" "" #\newline))
;; (define-scheme 'rscheme-rsc '("rsc") #f) ;; compiling
;; (define-scheme 'rhizome '("pisc") #f) ;; needs multiple executables ;-/
(define-scheme 'sagittarius '("sagittarius")
  #f
  (make-version-finder "--version" ", version " #\space))
;; (define-scheme 'scheme48 '("scheme48") #f) ;; todo: fix problems with loading script files
;; (define-scheme 'stalin-chicken '("chicken-stalin") (lambda (executable input-filename) (string-append executable " -On -Ob -Om -Or -Ot -d -d1 -k -copt -O3 " input-filename))) ;; compiling
(define-scheme 'tinyscheme '("tinyscheme") #f #f)
(define-scheme 'vicare '("vicare")
  #f
  (make-version-finder "--version" " version " #\,))
(define-scheme 'ypsilon '("ypsilon")
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
              (sexp boolean (#\s) "Output s-expressions")
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
  (let* ((sexp? (conf-get config '(command run sexp)))
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
                                          (let* ((command-line-maker (scheme-command-line-maker scheme))
                                                 (command-line (if command-line-maker (command-line-maker path input-file) (show #f path " " input-file))))
                                            (if sexp?
                                                (show #t "(" (magenta (symbol->string (scheme-name scheme))) nl)
                                                (show #t (magenta (symbol->string (scheme-name scheme))) nl))
                                            (let* ((before (current-jiffy))
                                                   (result (process->output+error+status command-line))
                                                   (after (current-jiffy))
                                                   (output (first result))
                                                   (error (second result))
                                                   (status (third result))
                                                   (get-version (scheme-get-version scheme))
                                                   (version (if get-version (get-version path scheme) "unknown")))
                                              (if sexp?
                                                  (begin
                                                    (show #t "  (" (yellow "file "))
                                                    (write input-file)
                                                    (show #t ")" nl)
                                                    (show #t "  (" (yellow "version "))
                                                    (write version)
                                                    (show #t ")" nl)
                                                    (show #t "  (" (yellow "stdout "))
                                                    (write output)
                                                    (show #t ")" nl)
                                                    (show #t "  (" (yellow "stderr "))
                                                    (write error)
                                                    (show #t ")" nl)
                                                    (show #t "  (" (yellow "time ") (inexact (/ (- after before) (jiffies-per-second))) ")" nl)
                                                    (show #t "  (" (yellow "status ") (if (zero? status) (green (number->string status)) (red (number->string status))) "))" nl)

                                                    )
                                                  (if (zero? status)
                                                      (show #t output nl (green " -> OK") nl)
                                                      (show #t error nl " " (scheme-name scheme) (red " -> ERROR") nl)))

                                              )))))
                                    schemes)))
                      input-files)
            (when sexp?
              (show #t ")" nl)))))))
