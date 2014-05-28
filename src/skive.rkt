#!racket

(require "compile.rkt"
	 "ffi.rkt")

(provide define-skive
         lambda-skive
         load-skive-from-file
         ;; provide compile functions too
         compile-skive-to-if1
         compile-if1-to-native)

;; TODO: compile sequence

(define-syntax define-skive
  (syntax-rules ()
    [(define-skive (name) . body)
     (define name
       (let* ((code (compile-sequence-to-if1 (quote body)))
	      (lib (compile-if1-to-native code #:type 'lib)))
	 (make-thunk lib)))]))

(define (lambda-skive source)
  (let* ((code (compile-skive-to-if1 source))
         (lib (compile-if1-to-native code #:type 'lib)))
    (make-thunk lib)))

(define (load-skive-from-file source)
  (let ((skive-code `(begin ,@(file->list source))))
    (lambda-skive skive-code)))
