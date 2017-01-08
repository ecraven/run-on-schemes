(cond-expand
  (chibi
    (import
      (scheme base)
      (scheme small)))
  (chicken #f))

(display "Hello, world!")
(newline)
