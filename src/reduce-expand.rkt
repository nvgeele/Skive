#!racket

(require "syntax.rkt")

(provide expand-reduce)

;; car := arguments needed
;; cdr := neutral element
(define reducible
  (hash '+ '(2 . 0)
	'- '(2 . 0)
	'* '(2 . 0)
	'/ '(2 . 0)))

(define (expand-reduce exp)
  (cond ((self-evaluating? exp)
	 exp)
	((let? exp)
	 (let* ((defs (let-definitions exp))
		(vars (map car defs))
		(args (map cadr defs))
		(body (let-body exp)))
	   `((lambda
	       ,vars
	       ,@(map expand-reduce body))
	     ,@args)))
	((application? exp)
	 (let ((red (hash-ref reducible (appl-op exp) #f))
	       (len (length (appl-args exp))))
	   (if red
	     (cond ((= len (car red))
		    `(,(appl-op exp) ,@(map expand-reduce (appl-args exp))))
		   ((< len (car red))
		    (error "Not enough arguments -- expand-reduce"))
		   ((> len (car red))
		    (reduce (appl-op exp) (appl-args exp)
			    (car red) (cdr red))))
	     (map expand-reduce exp))))
	(else (error (~a "Incorrect expression -- expand-reduce\n\t\""
			 exp "\"")))))

(define (reduce op args num-args neutral)
  (let loop ((res '())
	     (rest args))
    (let ((len (length rest)))
      (cond ((> len num-args)
	     (loop
	       (cons `(,op ,@(map expand-reduce (take rest num-args)))
		     res)
	       (list-tail rest num-args)))
	    ((= len num-args)
	     (reverse (cons `(,op ,@(map expand-reduce rest)) res)))
	    (else ;(< len num-args)
	      (reverse
		(cons `(,op ,@(append (map expand-reduce rest)
				      (make-list (- num-args len) neutral)))
		      res)))))))
