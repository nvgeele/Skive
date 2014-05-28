#!racket

(provide self-evaluating?
         quoted-symbol? quoted-symbol
	 application? appl-op appl-args
	 let? let-definitions let-body
	 lambda? lambda-args lambda-body
	 if? if-condition if-consequent if-alternative
	 or? and?
	 lexical-address? frame offset
         definition? definition-variable definition-value
         quote? quote-value
         letrec? letrec-definitions letrec-body
         begin? begin-expressions)

;;;; Auxiliary
(define (tagged-list? tag list)
  (and (list? list)
       (eq? (car list) tag)))

;;;; Begin
(define (begin? exp)
  (tagged-list? 'begin exp))

(define (begin-expressions exp)
  (cdr exp))

;;;; Self-evaluating
(define (self-evaluating? exp)
  (or (integer? exp)
      (real? exp)
      (string? exp)
      ;;(symbol? exp)
      (boolean? exp)
      (null? exp)
      (eq? exp #t)
      (false? exp)
      (quoted-symbol? exp)))

(define (quoted-symbol? exp)
  (and (tagged-list? 'quote exp)
       (symbol? (cadr exp))))

(define (quoted-symbol exp)
  (cadr exp))

;;;; Application
(define (application? exp)
  (list? exp))

(define (appl-op exp)
  (car exp))

(define (appl-args exp)
  (cdr exp))

;;;; Let expressions
(define (let? exp)
  (tagged-list? 'let exp))

(define (let-definitions exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

;;;; Lambda expressions
(define (lambda? exp)
  (tagged-list? 'lambda exp))

(define (lambda-args exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

;;;; If expressions
(define (if? exp)
  (tagged-list? 'if exp))

(define (if-condition exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (cadddr exp))

;;;; and/or
(define (or? exp)
  (tagged-list? 'or exp))

(define (and? exp)
  (tagged-list? 'and exp))

;;;; Lexical addressing
(define (lexical-address? exp)
  (vector? exp))

(define (frame exp)
  (vector-ref exp 0))

(define (offset exp)
  (vector-ref exp 1))

;;;; Define
(define (definition? exp)
  (tagged-list? 'define exp))

(define (definition-variable exp)
  (cadr exp))

(define (definition-value exp)
  (cddr exp))

;;;; Letrec
(define (letrec? exp)
  (tagged-list? 'letrec exp))

(define (letrec-definitions exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

;;;; Quote
(define (quote? exp)
  (tagged-list? 'quote exp))

(define (quote-value exp)
  (cadr exp))
