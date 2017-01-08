#!/usr/bin/chibi-scheme -r
;; set -x ANSI_ESCAPES_ENABLED 1
(import (chibi)
        (chibi filesystem) ;; file-exists? file-is-executable?
        (srfi 9) ;; define-record-type
        (chibi string) ;; string-suffix? string-split
        (srfi 98) ;; get-environment-variable
        (srfi 69) ;; hash tables
        (srfi 1) ;; filter-map
        (chibi show)
        (chibi process)
        (chibi term ansi)
        )

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

(define-scheme 'mit '("mit-scheme") (lambda (executable input-filename) (string-append executable " --quiet --load " input-filename)))
(define-scheme 'chez '("chez-scheme") (lambda (executable input-filename) (string-append executable " -q " input-filename)))
;; (define-scheme 'chicken '("csc") (lambda (executable input-filename) (string-append executable " -o /tmp/foo " input-filename " ; /tmp/foo")))
(define-scheme 'chicken-csi '("csi") (lambda (executable input-filename) (string-append executable " -q " input-filename)))
(define-scheme 'guile '("guile") #f)
(define-scheme 'gauche '("gosh") #f)

(define (find-schemes)
  (filter-map (lambda (scheme)
                (let ((executable (find (lambda (x) x) (map find-executable (scheme-executable-names scheme)))))
                  (if executable
                      (cons executable scheme)
                      #f)))
              (hash-table-values *schemes*)))

(define (main arguments)
  (let ((input-file (third arguments))
        (schemes (find-schemes)))
    (for-each (lambda (scheme)
                (let* ((path (car scheme))
                       (scheme (cdr scheme))
                       (command-line-maker (scheme-command-line-maker scheme))
                       (command-line (if command-line-maker (command-line-maker path input-file) (show #f path " " input-file))))
                  (show #t (magenta (symbol->string (scheme-name scheme))) nl)
                  (let* ((result (process->output+error+status command-line))
                         (output (first result))
                         (error (second result))
                         (status (third result)))
                    (if (zero? status)
                        (show #t output nl (green " -> OK") nl)
                        (show #t error nl " " (scheme-name scheme) (red " -> ERROR") nl)))))
              schemes)))
