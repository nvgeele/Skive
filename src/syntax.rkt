#!racket

(provide self-evaluating?
	 application? appl-op appl-args
	 let? let-definitions let-body
	 lambda? lambda-args lambda-body
	 if? if-condition if-consequent if-alternative
	 or? and?)

(define (self-evaluating? exp)
  (or (integer? exp)
      (string? exp)
      ;(symbol? exp)
      (boolean? exp)
      (null? exp)))

;;;; Application
(define (application? exp)
  (list? exp))

(define (appl-op exp)
  (car exp))

(define (appl-args exp)
  (cdr exp))

;;;; Let expressions
(define (let? exp)
  (and (list? exp)
       (eq? (car exp) 'let)))

(define (let-definitions exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

;;;; Lambda expressions
(define (lambda? exp)
  (and (list? exp)
       (eq? (car exp) 'lambda)))

(define (lambda-args exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

;;;; If expressions
(define (if? exp)
  (and (list? exp)
       (eq? (car exp) 'if)))

(define (if-condition exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (cadddr exp))

;;;; and/or
(define (or? exp)
  (and (list? exp)
       (eq? (car exp) 'or)))

(define (and? exp)
  (and (list? exp)
       (eq? (car exp) 'and)))
