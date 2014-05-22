#lang racket

(require "typing.rkt")

(provide get-runtime-boundaries)

(define runtime-list
  '(cons
    divide
    equal
    get_car
    get_cdr
    ;;is_bool
    ;;is_cons
    ;;is_false
    is_false_nat
    ;;is_float
    ;;is_int
    is_list
    is_list_intern
    ;;is_null
    ;;is_num
    ;;is_string
    ;;is_symbol
    ;;is_vector
    minus
    multiply
    plus
    apply
    map-vector
    vector-ref
    vector-length
    ))

(define (get-runtime-boundaries)
  (let*-values
      ([(path ignore ignore2) (split-path (current-contract-region))]
       [(path) (string-append (path->string path) "runtime/")])
    (parameterize ([current-load-relative-directory path])
      (for/list ([func runtime-list])
        (eval `(dynamic-require ,(~a func ".rkt") ',func))))))
