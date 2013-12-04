#!racket

(require "compile.rkt"
	 "ffi.rkt")

(provide define-skive)

(define-syntax define-skive
  (syntax-rules ()
    [(define-skive (name) . body)
     (define name
       (let* ((code (compile-skive-to-if1 (car (quote body))))
	      (lib (compile-if1-to-native code #:type 'lib)))
	 (make-thunk lib)))]))
