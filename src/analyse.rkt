#!racket

(require "syntax.rkt"
         "natives.rkt")

(provide analyse)

(define (analyse exp . env)
  (let ((env (if (null? env)
                 (cons '() (map car natives-list))
                 (car env))))
    (cond ((symbol? exp)
           (if (eq? exp 'null)
               '()
               (lexical-address env exp)))
          ((or (self-evaluating? exp)
               (quote? exp))
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
           `(,(if (lookup-var env (appl-op exp))
                  (lexical-address env (appl-op exp))
                  (case (appl-op exp)
                    [(list) 'list]
                    [(vector) 'vector]
                    [else (analyse (appl-op exp) env)]))
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

(define (lookup-var env exp)
  (let loop ((env env)
             (frame 0))
    (let ((idx (list-index exp (cdr env))))
      (cond (idx (cons frame idx))
            ((null? (car env)) #f)
            (else (loop (car env) (+ frame 1)))))))

(define (lexical-address env exp)
  (let ((lookup (lookup-var env exp)))
    (if lookup
        `#(,(car lookup)
           ,(cdr lookup))
        (error (~a "Symbol `" exp "' not found in env -- analyse")))))
