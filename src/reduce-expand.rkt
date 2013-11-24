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
	     ,@(map expand-reduce args))))
	((or? exp)
	 (expand-reduce (expand-or exp)))
	((and? exp)
	 (expand-reduce (expand-and exp)))
	((application? exp)
	 (let ((red (hash-ref reducible (appl-op exp) #f))
	       (len (length (appl-args exp))))
	   (if red
	     (cond ((= len (car red))
		    `(,(appl-op exp) ,@(map expand-reduce (appl-args exp))))
		   ((< len (car red))
		    (error "Not enough arguments -- expand-reduce"))
		   ((> len (car red))
		    (expand-reduce
		      (reduce (appl-op exp) (appl-args exp)
			      (car red) (cdr red)))))
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
			(,op ,@(take rest num-args))) ;,@(map expand-reduce (take rest num-args))))
		     res)
	       (list-tail rest num-args)
	       (+ i 1)))
	    ((= len num-args)
	     `(let ,(cons `(,(string->symbol (~a "let_" i))
			     (,op ,@(take rest num-args))) ;,@(map expand-reduce (take rest num-args))))
			  res)
		,(reduce* `(,op ,@(map (lambda (i)
					 (string->symbol (~a "let_" i)))
				       (range 0 (+ i 1))))
			  num-args neutral)))
	    (else ;(< len num-args)
	      (if (null? res)
		`(,op ,@(append rest ;(map expand-reduce rest)
				(make-list (- num-args len) neutral)))
		`(let ,(cons `(,(string->symbol (~a "let_" i))
				(,op ,@(append rest ;(map expand-reduce rest)
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

;; We could avoid this nested mess of let-expressions in IF1,
;; but that would change the behaviour of or. By doing or/and
;; like this, the functions will be: 1) lazy and 2) have
;; return values that aren't booleans. The Scheme behaviour.
;; However, this has a higher impact on performance...
(define (expand-or exp)
  (let do-expand ((args (cdr (reduce* exp 2 #f)))
		  (i 0))
    (let ((sym (string->symbol (~a "let_" i))))
      `(let ((,sym ,(car args)))
	 (if ,sym
	   ,sym
	   ,(if (or? (cadr args))
	      (do-expand (cdadr args) (+ i 1))
	      (cadr args)))))))

;; Why do we always +1 the symbol? So we don't need to go
;; back to previous frames.
(define (expand-and exp)
  (let do-expand ((args (cdr (reduce* exp 2 #t)))
		  (i 0))
    (let ((sym1 (string->symbol (~a "let_" i)))
	  (sym2 (string->symbol (~a "let_" (+ i 1)))))
      `(let ((,sym1 ,(car args)))
	 (if ,sym1
	   (let ((,sym2 ,(if (and? (cadr args))
			   (do-expand (cdadr args) (+ i 2))
			   (cadr args))))
	     ,sym2)
	   #f)))))

;;; (expand-reduce '(let ((a 1) (b 2)) (* a b (let ((c 1)) c)) ((lambda (x) x) 1)))
;;; (let ((exp (expand-reduce '(or 1 2 3 3 4)))) (display exp)(newline)(eval exp))
;;; (let ((exp (expand-reduce '(or 1 2 3 3 (display "test"))))) (display exp)(newline)(eval exp))
