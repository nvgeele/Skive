#!racket

(require "syntax.rkt"
	 "natives.rkt")

(provide analyse)

(define (analyse exp . env)
  (let ((env (if (null? env)
	       (cons '() (map car natives-list))
	       (car env))))
    (cond ((symbol? exp)
	   (lookup-var exp env))
	  ((self-evaluating? exp)
	   exp)
	  ((if? exp)
	   `(if ,(analyse (if-condition exp) env)
	      ,(analyse (if-consequent exp) env)
	      ,(analyse (if-alternative exp) env)))
	  ((lambda? exp)
	   (let ((env (cons env
			    (lambda-args exp))))
	     `(lambda ,(lambda-args exp)
		,@(analyse (lambda-body exp) env))))
	  ((application? exp)
	   `(,(analyse (appl-op exp) env)
	     ,@(map (lambda (arg)
		      (analyse arg env))
		    (appl-args exp))))
	  (else (error "Incorrect expression -- analyse")))))

(define (list-index el lst)
  (if (null? lst)
    #f
    (let loop ((list lst)
	       (idx 0))
      (cond ((eq? (car list) el) idx)
	    ((null? (cdr list)) #f)
	    (else (loop (cdr list) (+ idx 1)))))))

(define (lookup-var exp env)
  (let loop ((env env)
	     (frame 0))
    (let ((idx (list-index exp (cdr env))))
      (cond (idx
	     `#(,frame ,idx))
	    ((null? (car env))
	     (error (~a "Symbol `" exp "' not found in env -- analyse")))
	    (else (loop (car env) (+ frame 1)))))))
