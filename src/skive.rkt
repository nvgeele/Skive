#!racket

(require "compile.rkt"
	 "ffi.rkt")

(provide define-skive
         lambda-skive)

;; TODO: compile sequence

(define-syntax define-skive
  (syntax-rules ()
    [(define-skive (name) . body)
     (define name
       (let* ((code (compile-skive-to-if1 (car (quote body))))
	      (lib (compile-if1-to-native code #:type 'lib)))
	 (make-thunk lib)))]))

(define (lambda-skive source)
  (let* ((code (compile-skive-to-if1 source))
         (lib (compile-if1-to-native code #:type 'lib)))
    (make-thunk lib)))
