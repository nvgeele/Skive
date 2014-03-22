#lang racket

(require "typing.rkt")

(provide generate-runtime-code)

(define runtime-list
  `(;;is_num
    ;;is_int
    ;;is_float
    ;;is_null
    ;;is_string
    ;;is_bool
    ;;is_cons
    ;;is_false
    get_car
    get_cdr
    rt-cons
    plus
    minus
    multiply
    divide
    equal
    is_false_nat))

(define (generate-runtime-code)
  (foldl
   (lambda (proc str)
     (string-append
      str
      (file->string (string-append "./if1/" (symbol->string proc) ".if1"))))
   ""
   runtime-list))
