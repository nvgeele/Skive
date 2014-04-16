#lang racket

(require "../../src/ffi.rkt")

(define tokens
  '((lpar)
    (num 5)
    (col)
    (st)
    (lpar)
    (num 1)
    (col)
    (num 25)
    (rpar)
    (lpar)
    (num 5)
    (col)
    (st)
    (lpar)
    (num 3)
    (col)
    (string "fib")
    (rpar)
    (lpar)
    (num 1)
    (col)
    (num 121393)
    (rpar)
    (gt)
    (rpar)
    (gt)
    (rpar)))

(define (go)
  (parse tokens))

(define fibre
  "( 0 : nil)")

(define (go2)
  (parse-fibre-input fibre))
