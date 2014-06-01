#!racket

(require "syntax.rkt"
	 "natives.rkt")

(provide expand)

(define (expand exp)
  (cond ((or (self-evaluating? exp)
	     (symbol? exp))
	 exp)
        ((vector? exp)
         (expand `(vector ,@(vector->list exp))))
        ((begin? exp)
         (expand (transform-definitions (cdr exp))))
        ((definition? exp)
         (error "Definitions only allowed inside begin expressions -- expand"))
	((let? exp)
	 (let* ((defs (let-definitions exp))
		(vars (map car defs))
		(args (map cadr defs))
		(body (let-body exp)))
	   `((lambda
                 ,vars
 	       ,@(map expand body))
	     ,@(map expand args))))
        ((let*? exp)
         (let ((defs (reverse (let-definitions exp)))
               (body (let-body exp)))
           (expand
            (car
             (foldl (lambda (def exp)
                      `((let (,def)
                          ,@exp)))
                    body
                    defs)))))
        ((letrec? exp)
         (expand (expand-letrec exp)))
	((or? exp)
	 (expand (expand-or exp)))
	((and? exp)
	 (expand (expand-and exp)))
	((if? exp)
	 (expand-if exp))
        ((quote? exp)
         exp)
	((application? exp)
         (map expand exp))
	(else (error (~a "Incorrect expression -- expand\n\t\""
			 exp "\"")))))

;; Tracks down the define's in a sequence and puts them
;; in a letrec expression.
(define (transform-definitions sequence)
  (let ((expressions
         (foldl (lambda (exp exps)
                  (if (definition? exp)
                      (cons (cons (if (list? (definition-variable exp))
                                      `(,(car (definition-variable exp))
                                        (lambda ,(cdr (definition-variable exp))
                                          ,@(definition-value exp)))
                                      `(,(definition-variable exp)
                                        (begin ,@(definition-value exp))))
                                  (car exps))
                            (cdr exps))
                      (cons (car exps)
                            (cons exp (cdr exps)))))
                (cons '() '())
                sequence)))
    `(letrec ,(car expressions)
       ,@(reverse (cdr expressions)))))

(define (expand-begin exp)
  `((lambda () ,@(begin-expressions exp))))

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

(define (expand-if exp)
  `(if ,(expand (if-condition exp))
       ,(expand (if-consequent exp))
       ,(expand (if-alternative exp))))

(define (expand-definition exp)
  (if (list? (definition-variable exp))
      `(define ,(car (definition-variable exp))
         (lambda ,(cdr (definition-variable exp))
           ,@(map expand (definition-value exp))))
      `(define ,(definition-variable exp)
         ,(expand (car (definition-value exp))))))

(define (build-recursion-list* names exp)
  (cond ((or (self-evaluating? exp)
             (symbol? exp))
         (if (member exp names)
             `(,exp)
             '()))
        ((quote? exp)
         '())
        ((application? exp)
         (apply append
                (map (lambda (exp)
                       (build-recursion-list* names exp))
                     (cons (appl-op exp)
                           (appl-args exp)))))
        (else "Unknown error")))

(define (build-recursion-list def names)
  (let ((name (car def))
        (body (cadr def)))
    (if (lambda? body)
        (cons name
              (build-recursion-list*
               (set-subtract names (lambda-args body))
               (lambda-body body)))
        (cons name '()))))

(define (build-argument-lists recursion-lists)
  (for/list ([func recursion-lists])
    (let ((name (car func))
          (calls (cdr func)))
      (cons name
            (if (null? calls)
                '()
                (set->list
                 (list->set (append calls
                                    (flatten
                                     (map (lambda (n)
                                            (cdr (assoc n recursion-lists)))
                                          calls))))))))))

(define (replace-recursive-calls exp names argument-lists)
  (cond ((or (self-evaluating? exp)
             (symbol? exp))
         exp)
        ((quote? exp)
         exp)
        ((application? exp)
         (if (member (appl-op exp) names)
             `(,(appl-op exp)
               ,@(map (lambda (exp)
                        (replace-recursive-calls exp names argument-lists))
                      (appl-args exp))
               ,@(cdr (assoc (appl-op exp) argument-lists)))
             `(,(replace-recursive-calls (appl-op exp) names argument-lists)
               ,@(map (lambda (exp)
                        (replace-recursive-calls exp names argument-lists))
                      (appl-args exp)))))
        (else "Unknown error")))

(define (adapt-definitions definitions argument-lists names)
  (for/list ([def definitions])
    (let* ((name (car def))
           (arg-list (cdr (assoc name argument-lists))))
      (if (null? arg-list)
          (cons def '())
          (let* ((original-arg-list
                  (lambda-args (cadr def)))
                 (full-arg-list
                  (append original-arg-list arg-list)))
            (cons `(,name (lambda ,full-arg-list
                            ,@(map (lambda (exp)
                                     (replace-recursive-calls exp
                                                              names
                                                              argument-lists))
                                   (lambda-body (cadr def)))))
                  `(,name (lambda ,original-arg-list
                            (,name ,@full-arg-list)))))))))

;; How does this expansion work?
;; First, we iterate over all definitions (build-recursion-list).
;; If a definition is a function, we perform some kind of abstract
;; evaluation (build-recursion-list*) to see if any (mutually)
;; recursive calls occur, or another definition from the list is
;; referenced in the function body. (build-argument-lists) uses
;; this information to build a list of arguments the partial
;; functions need. This information is then again used in
;; (adapt-definitions), which will generate definitions for the
;; partial functions (if needed) by replacing recursive calls to
;; make sure all the correct parameters will be passed to make
;; possible recursive calls. The result of the expansion is two
;; nested let-expressions.

(define (expand-letrec exp)
  (let* ((names (map car (letrec-definitions exp)))
         (recursion-lists (map (lambda (def)
                                 (build-recursion-list def names))
                               (letrec-definitions exp)))
         (argument-lists (build-argument-lists recursion-lists))
         (definitions (adapt-definitions (letrec-definitions exp)
                                         argument-lists
                                         names)))
    `(let ,(map car definitions)
       (let ,(filter (compose not null?)
                     (map cdr definitions))
         ,@(letrec-body exp)))))
