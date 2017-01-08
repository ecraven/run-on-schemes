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
        (srfi 1) ;; filter-map
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
  (make-scheme name executable-names command-line-maker)
  scheme?
  (name scheme-name)
  (executable-names scheme-executable-names)
  (command-line-maker scheme-command-line-maker))

(define (define-scheme name executable-names command-line-maker)
  (hash-table-set! *schemes* name (make-scheme name executable-names command-line-maker)))

;;;; Known Scheme implementations
;; (define-scheme 'bigloo ...) ;; compiling
;; (define-scheme 'bones ...) ;; compiling
(define-scheme 'chez '("chez-scheme") (lambda (executable input-filename) (string-append executable " -q " input-filename)))
(define-scheme 'chibi '("chibi-scheme") #f)
;; (define-scheme 'chicken '("csc") (lambda (executable input-filename) (string-append executable " -o /tmp/foo " input-filename " ; /tmp/foo"))) ;; compiling
(define-scheme 'chicken-csi '("csi") (lambda (executable input-filename) (string-append executable " -q " input-filename)))
(define-scheme 'cyclone '("cyclone") #f)
(define-scheme 'foment '("foment") #f)
;; (define-scheme 'gambitc ...) ;; compiling
(define-scheme 'gauche '("gosh") #f)
(define-scheme 'guile '("guile") #f)
(define-scheme 'ironscheme '("ironscheme") #f)
(define-scheme 'kawa '("kawa") #f)
;; (define-scheme 'larceny '("larceny") #f) ;; todo: fix problems with loading script files
(define-scheme 'mit '("mit-scheme") (lambda (executable input-filename) (string-append executable " --quiet --no-init-file --load " input-filename)))
(define-scheme 'mosh '("mosh-scheme") #f)
(define-scheme 'owl-lisp '("ol") #f)
;; (define-scheme 'petit-larceny '("larceny") #f) ;; todo
(define-scheme 'petite-chez '("petite") #f)
(define-scheme 'racket '("racket") #f)
(define-scheme 'rscheme '("fshell") (lambda (executable input-filename) (string-append executable " -q " input-filename)))
;; (define-scheme 'rscheme-rsc '("rsc") #f) ;; compiling
;; (define-scheme 'rhizome '("pisc") #f) ;; needs multiple executables ;-/
(define-scheme 'sagittarius '("sagittarius") #f)
;; (define-scheme 'scheme48 '("scheme48") #f) ;; todo: fix problems with loading script files
;; (define-scheme 'stalin-chicken '("chicken-stalin") (lambda (executable input-filename) (string-append executable " -On -Ob -Om -Or -Ot -d -d1 -k -copt -O3 " input-filename))) ;; compiling
(define-scheme 'tinyscheme '("tinyscheme") #f)
(define-scheme 'vicare '("vicare") #f)
(define-scheme 'ypsilon '("ypsilon") #f)

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
           (@ (scheme string (#\r) "Run only matching Schemes")
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
  (let ((sexp? (conf-get config '(command run sexp)))
        (no-color? (conf-get config '(command run no-color))))
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
                                             (scheme (cdr scheme))
                                             (command-line-maker (scheme-command-line-maker scheme))
                                             (command-line (if command-line-maker (command-line-maker path input-file) (show #f path " " input-file))))
                                        (when path
                                          (if sexp?
                                              (show #t "(" (magenta (symbol->string (scheme-name scheme))) nl)
                                              (show #t (magenta (symbol->string (scheme-name scheme))) nl))
                                          (let* ((before (current-jiffy))
                                                 (result (process->output+error+status command-line))
                                                 (after (current-jiffy))
                                                 (output (first result))
                                                 (error (second result))
                                                 (status (third result)))
                                            (if sexp?
                                                (begin
                                                  (show #t "  (" (yellow "file "))
                                                  (write input-file)
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

                                            ))))
                                    schemes)))
                      input-files)
            (when sexp?
              (show #t ")" nl)))))))
