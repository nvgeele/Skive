#!racket

(require "node.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt")

(provide compile)

;; Currently parses all into one boundary
;; => needs to be program when typing and
;; environments etc are implemented(?)

(define (compile code)
  (let-values ([(gb result-node)
		(parse (make-graph-boundary "main") code)]); '(0 1))])
    (display result-node)(newline)
    (display gb)))

(define (parse boundary exp); linkage)
  (cond ((self-evaluating? exp)
	 (parse-self-evaluating boundary exp))
	((application? exp)
	 (parse-application boundary exp)); linkage))
	(else (error "Incorrect expression"))))

(define (self-evaluating? exp)
  (or (integer? exp)))

(define (parse-self-evaluating boundary exp)
  (let ((node (make-node `(literal ,exp))))
    (let-values ([(boundary label) (add-node boundary node)])
      (values boundary label))))

(define (application? exp)
  (list? exp))

(define (parse-application boundary exp); linkage)
  (let ((operator (car exp))
	(operands (cdr exp)))
    (if (hash-ref natives operator #f)
      (let*-values ([(boundary inputs)
		     (parse-operands boundary operands)]
		    [(boundary oplabel)
		     (add-node boundary (hash-ref natives operator))])
	(values (car (foldl (lambda (input boundary)
			      (cons (add-edge (car boundary)
					      input 1
					      oplabel (cdr boundary))
				    (+ 1 (cdr boundary))))
			    (cons boundary 1)
			    inputs))
		oplabel))
      (error "Only native functions supported!"))))

;; Accepts a graph-boundary and a list of operands.
;; Returns boundary with nodes/edges of operands added
;; and a list of the operands' node labels.
(define (parse-operands boundary operands)
  (if (null? operands)
    (values boundary '())
    (let loop ((boundary boundary)
	       (inputs '())
	       (cur (car operands))
	       (rem (cdr operands)))
      (let-values ([(gb link) (parse boundary cur)])
	(if (null? rem)
	  (values gb (reverse (cons link inputs)))
	  (loop gb (cons link inputs) (car rem) (cdr rem)))))))
