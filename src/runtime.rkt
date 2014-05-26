#lang racket

(require "typing.rkt")

;; All files containing the runtime functions
(require "runtime/cons.rkt"
         "runtime/divide.rkt"
         "runtime/equal.rkt"
         "runtime/get_car.rkt"
         "runtime/get_cdr.rkt"
         "runtime/is_false_nat.rkt"
         "runtime/is_list.rkt"
         "runtime/is_list_intern.rkt"
         "runtime/minus.rkt"
         "runtime/multiply.rkt"
         "runtime/plus.rkt"
         "runtime/apply.rkt"
         "runtime/map-vector.rkt"
         "runtime/vector-ref.rkt"
         "runtime/vector-set.rkt"
         "runtime/vector-length.rkt"
         "runtime/make-vector.rkt"
         "runtime/error.rkt"
         "runtime/is_null.rkt")

(provide get-runtime-boundaries)

(define runtime-list
  (list cons
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
        is_null
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
        vector-set
        vector-length
        make-vector
        error
        ))

(define (get-runtime-boundaries)
  runtime-list)
