#!racket

(require "node.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt")

(provide parse)

(define (parse exp)
  (cond ((self-evaluating? exp)
	 (parse-self-evaluating exp))
	((application? exp)
	 (parse-application exp))
	(else (error "Incorrect expression"))))

(define (self-evaluating? exp)
  (or (integer? exp)))

(define (parse-self-evaluating exp)
  '())

(define (application? exp)
  (list? exp))

(define (parse-application exp)
  '())
