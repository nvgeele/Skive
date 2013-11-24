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
	     (rest args)
	     (i 0))
    (let ((len (length rest)))
      (cond ((> len num-args)
	     (loop
	       (cons `(,(string->symbol (~a "let_" i))
			(,op ,@(map expand-reduce (take rest num-args))))
		     res)
	       (list-tail rest num-args)
	       (+ i 1)))
	    ((= len num-args)
	     `(let ,(cons `(,(string->symbol (~a "let_" i))
			     (,op ,@(map expand-reduce (take rest num-args))))
			  res)
		,(reduce* `(,op ,@(map (lambda (i)
					 (string->symbol (~a "let_" i)))
				       (range 0 (+ i 1))))
			  num-args neutral)))
	    (else ;(< len num-args)
	      (if (null? res)
		`(,op ,@(append (map expand-reduce rest)
				(make-list (- num-args len) neutral)))
		 `(let ,(cons `(,(string->symbol (~a "let_" i))
				 (,op ,@(append (map expand-reduce rest)
						(make-list (- num-args len) neutral))))
			      res)
		    ,(reduce* `(,op ,@(map (lambda (i)
					     (string->symbol (~a "let_" i)))
					   (range 0 (+ i 1))))
			      num-args neutral))))))))

(define (reduce* exp num-args neutral)
  (let ((op (car exp))
	(args (cdr exp)))
    (let loop ((rem args))
      (let ((len (length rem)))
	(cond ((= len num-args)
	       `(,op ,@rem))
	      ((< len num-args)
	       `(,op ,@rem
		     ,@(make-list (- num-args len) neutral)))
	      ((> len num-args)
	       `(,op ,@(take rem (- num-args 1))
		     ,(loop (drop rem (- num-args 1))))))))))

;;; (expand-reduce '(let ((a 1) (b 2)) (* a b (let ((c 1)) c)) ((lambda (x) x) 1)))
