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
