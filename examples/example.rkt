#lang racket

;; Require the Skive compiler + extras
(require "../src/skive.rkt")

;; We can define Skive functions with macros
(define-skive (calculate-answer)
  (* 2 (* 3 7)))

(displayln
 (~a "Answer to life, the universe, and everything: "
     (calculate-answer)))

(displayln "-----------")

;; We can load Skive code from a file and wrap it in a thunk
(define factorial
  (load-skive-from-file "factorial.skv"))

(define matrix
  (load-skive-from-file "matrix.skv"))

(displayln
 (~a "Result from factorial.skv: "
     (factorial)))

(displayln
 (~a "Result from matrix.skv: "
     (matrix)))

(displayln "-----------")

;; We can generate code at runtime, compile and wrap it
(define (generate-thunk n)
  (lambda-skive `(* ,n ,n)))

(displayln
 (~a "Result from code generation: "
     (+ ((generate-thunk 10))
        ((generate-thunk 42)))))

;; Getting only IF1 code is possible too
(define if1-code
  (let ((skive-code `(begin ,@(file->list "factorial.skv"))))
    (compile-skive-to-if1 skive-code)))

;; To compile the code manually, you can use compile-if1-to-native
;; e.g.
;; (compile-if1-to-native if1-code)
;; (compile-if1-to-native if1-code #:path "/tmp/s.out")
