#!racket

(provide self-evaluating?
	 application? appl-op appl-args
	 let? let-definitions let-body)

(define (self-evaluating? exp)
  (or (integer? exp)
      (string? exp)
      (symbol? exp)
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
