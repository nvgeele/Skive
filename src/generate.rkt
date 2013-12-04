#!racket

(require "threading.rkt"
	 "syntax.rkt"
	 "node.rkt"
	 "edge.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "typing.rkt"
	 "program.rkt")

(provide generate)

(define (generate exp)
  (let ((program (make-program))
	(boundary (make-graph-boundary "entry")))
    (let-values ([(program gb res) (generate* exp boundary program)])
      (add-boundary program
		    (add-edge gb res 1 0 1 typedval-lbl)))))

(define (generate-sequence seq)
  '())

(define (generate-sequence* seq graph-boundary program)
  (let loop ((cur (car seq))
	     (rem (cdr seq))
	     (gb graph-boundary)
	     (program program))
    (let-values ([(program gb res)
		  (generate* cur gb program)])
      (if (null? rem)
	(values program gb res)
	(loop (car rem) (cdr rem) gb program)))))

;; Always returns the following multiple values:
;; - program
;; - graph boundary
;; - result node
(define (generate* exp graph-boundary program)
  (cond ((lexical-address? exp)
	 (let-values ([(gb res) (generate-lookup graph-boundary
						 (vector-ref exp 0)
						 (vector-ref exp 1))])
	   (values program gb res)))
	((self-evaluating? exp)
	 (let-values ([(gb  res) (generate-self-evaluating graph-boundary exp)])
	   (values program gb res)))
	((lambda? exp)
	 (generate-lambda exp graph-boundary program))
	((application? exp)
	 (generate-application exp graph-boundary program))
	(else (error "Incorrect expression -- generate"))))

(define (generate-lookup graph-boundary frame offset)
  (let loop ((frame frame)
	     (gb graph-boundary)
	     (res 0))
    (if (= frame 0)
      (let*-values ([(gb frm-elm) (add-node gb (make-simple-node 144))]
		    [(gb bnd-lit) (add-node gb (make-literal-node offset))]
		    [(gb bnd-elm) (add-node gb (make-simple-node 105))])
	(values (~> gb
		    (add-edge res 1 frm-elm 1 frame-lbl)
		    (add-edge bnd-lit 1 bnd-elm 2 int-lbl)
		    (add-edge frm-elm frame-bind-idx bnd-elm 1 typedval-array-lbl))
		bnd-elm))
      (let*-values ([(gb-fail) (make-graph-boundary "")]
		    [(gb-fail lit-err) (add-node gb-fail (make-literal-node "error"))]
		    [(gb-fail) (add-edge gb-fail lit-err 1 0 1 frame-lbl)]
		    
		    [(gb-succ) (~> (make-graph-boundary "")
				   (add-edge 0 1 0 1 frame-lbl))]

		    [(gb frm-elm) (add-node gb (make-simple-node 144))]
		    [(gb tagcase) (add-node gb (make-tagcase ;typedval-type-count
						 `(,gb-fail ,gb-succ ,gb-fail)
						 #(1 0)))]) ;; Assuming back-frame-idx is 1
	(loop (- frame 1)
	      (~> gb
		  (add-edge res 1 frm-elm 1 frame-lbl)
		  (add-edge frm-elm frame-prev-idx tagcase 1 back-lbl))
	      tagcase)))))


      
(define (generate-lookup* graph-boundary frame offset)
  (let loop ((frame frame)
	     (gb graph-boundary)
	     (res 0))
    (if (= frame 0)
      (let*-values ([(gb res1) (add-node gb (make-simple-node 144))]  ;; RElements
		    [(gb res2) (add-node gb (make-simple-node 105))]  ;; AElement
		    [(gb lit1) (add-node gb (make-literal-node offset))])
	#|(values (add-edge (add-edge gb res 1 ;; Assuming frame-idx is always 1!
				      res1 1 frame-lbl)
			    res1 frame-bind-idx
			    res2 1 typedval-array-lbl)
		  res2))|#
      (values (~> gb
		  (add-edge res 1 res1 1 frame-lbl) ;; Assuming back-frame-idx is always 1!
		  (add-edge res1 frame-bind-idx res2 1 typedval-array-lbl)
		  (add-edge lit1 1 res2 2 int-lbl))
	      res2))
    (let*-values ([(gb res1) (add-node gb (make-simple-node 144))]  ;; RElements
		  [(gb res2) (add-node gb (make-simple-node 144))]) ;; RElements
      (loop (- frame 1)
	    (add-edge (add-edge gb res 1 ;; Assuming back-frame-idx is always 1!
				res1 1 frame-lbl)
		      res1 frame-prev-idx
		      res2 1 back-lbl)
	    res2)))))

(define (generate-self-evaluating graph-boundary exp)
  (let*-values ([(lit-node build-node) (values (make-literal-node exp)
					       (make-simple-node 143))]
		[(type-lbl type-idx) (cond ((integer? exp) (values int-lbl
								   typedval-int-idx))
					   ((string? exp) (values string-lbl
								  typedval-string-idx))
					   (else (error "Unknown error")))]
		[(gb blbl) (add-node graph-boundary build-node)]
		[(gb llbl) (add-node gb lit-node)]
		[(gb) (add-edge gb llbl 1 blbl type-idx type-lbl)])
    (values gb blbl)))

(define (generate-lambda exp graph-boundary program)
  (let*-values ([(boundary) (make-graph-boundary "temp")]
		[(program gb res) (generate-sequence* (lambda-body exp) boundary program)]
		[(gb) (add-edge gb res 1 0 1 typedval-lbl)]
		[(label) (next-label program)]
		[(str-label) (next-label-str program)]
		[(program) (add-boundary program (set-boundary-name gb str-label))]
		[(gb lit1) (add-node graph-boundary (make-literal-node label))]
		[(gb lit2) (add-node gb (make-literal-node (length (lambda-args exp))))]
		[(gb bld1) (add-node gb (make-simple-node 143))]
		[(gb bld2) (add-node gb (make-simple-node 143))])
    (values program
	    (~> gb
		(add-edge lit1 1 bld1 closure-func-idx int-lbl)
		(add-edge lit2 1 bld1 closure-args-idx int-lbl)
		(add-edge lit2 1 bld1 closure-framesize-idx int-lbl)
		(add-edge 0    1 bld1 closure-env-idx frame-lbl)
		(add-edge bld1 1 bld2 typedval-func-idx closure-lbl))
	    bld2)))

#|(define (generate-args arg-exps graph-boundary program)
  (if (null? arg-exps)
    (values program graph-boundary '())
    (let loop ((cur (car arg-exps))
	       (rem (cdr arg-exps))
	       (lst '())
	       (gb graph-boundary)
	       (program program))
      (let-values ([(program gb res) (generate* cur gb program)])
	(if (null? rem)
	  (values program gb (reverse (cons res lst)))
	  (loop (car rem) (cdr rem) (cons res lst) gb program))))))

(define (generate-application exp graph-boundary program)
  (let*-values ([(program gb op-res)
		 (generate* (appl-op exp) graph-boundary program)]
		[(program gb args)
		 (generate-args (appl-args exp) gb program)]
		[(gb bnd-bld) (add-node gb (make-simple-node 103))] ;; ABuild
		[(gb tvl-elm) (add-node gb (make-simple-node 144))] ;; RElem
		[(gb cls-elm) (add-node gb (make-simple-node 144))] ;; RElem
		[(gb bck-bld) (add-node gb (make-simple-node 143))] ;; RBuild	
		[(gb frm-bld) (add-node gb (make-simple-node 143))] ;; RBuild       
		[(gb lit1) (add-node gb (make-literal-node "call"))]
		[(gb call) (add-node gb (make-simple-node 120))]    ;; call
		[(gb) (car (foldl (lambda (node c)
				    (cons (add-edge (car c) node 1 bnd-bld (cdr c) typedval-lbl)
					  (+ 1 (cdr c))))
				  (cons gb 2) args))]
		[(gb) (~> gb
			  (add-edge op-res 1 tvl-elm 1 typedval-lbl)
			  (add-edge tvl-elm typedval-func-idx cls-elm 1 closure-lbl)
			  (add-edge cls-elm closure-framesize-idx bnd-bld 1 int-lbl)
			  (add-edge cls-elm closure-env-idx bck-bld back-frame-idx frame-lbl)
			  (add-edge bck-bld 1 frm-bld frame-prev-idx back-lbl);;fix?
			  (add-edge bnd-bld 1 frm-bld frame-bind-idx typedval-array-lbl)
			  (add-edge cls-elm closure-func-idx call 2 int-lbl)
			  (add-edge frm-bld 1 call 3 frame-lbl)
			  (add-edge lit1 1 call 1 call-function-lbl))])
    (values program gb call)))|#

(define (generate-args arg-exps graph-boundary program)
  (if (null? arg-exps)
    (values program graph-boundary '())
    (let loop ((cur (car arg-exps))
	       (rem (cdr arg-exps))
	       (lst '())
	       (gb graph-boundary)
	       (program program))
      (let-values ([(program gb res) (generate* cur gb program)])
	(if (null? rem)
	  (values program gb (reverse (cons res lst)))
	  (loop (car rem) (cdr rem) (cons res lst) gb program))))))

(define (generate-application exp graph-boundary program)
  (let*-values ([(gb-call) (make-graph-boundary "")]
		[(gb-fail) (make-graph-boundary "")]
		[(num-args) (length (appl-args exp))]

		[(program gb op-res)
		 (generate* (appl-op exp) graph-boundary program)]
		[(program gb args)
		 (generate-args (appl-args exp) gb program)]

		;[(gb-call tvl-elm) (add-node gb-call (make-simple-node 144))]
		[(gb-call cls-elm) (add-node gb-call (make-simple-node 144))]
		[(gb-call bnd-lit) (add-node gb-call (make-literal-node "0"))]
		[(gb-call bnd-bld) (add-node gb-call (make-simple-node 103))]
		[(gb-call bck-bld) (add-node gb-call (make-simple-node 143))]
		[(gb-call frm-bld) (add-node gb-call (make-simple-node 143))]
		[(gb-call cll-lit) (add-node gb-call (make-literal-node "call"))]
		[(gb-call call)    (add-node gb-call (make-simple-node 120))]

		[(gb-call) (~> gb-call
			       (add-edge 0 1 cls-elm 1 closure-lbl)
			       (add-edge bnd-lit 1 bnd-bld 1 int-lbl)
			       (add-edge cls-elm closure-env-idx bck-bld back-frame-idx frame-lbl)
			       (add-edge bnd-bld 1 frm-bld frame-bind-idx typedval-array-lbl)
			       (add-edge bck-bld 1 frm-bld frame-prev-idx back-lbl)
			       (add-edge cls-elm closure-func-idx call 2 int-lbl)
			       (add-edge frm-bld 1 call 3 frame-lbl)
			       (add-edge cll-lit 1 call 1 call-function-lbl)
			       (add-edge call 1 0 1 typedval-lbl))]

		[(gb-call) (foldl (lambda (p gb)
				    (add-edge gb 0 p bnd-bld p typedval-lbl))
				  gb-call
				  (range 2 (+ 2 num-args)))]

		[(gb-fail err-lit) (add-node gb-fail (make-literal-node "error"))]
		[(gb-fail) (add-edge gb-fail err-lit 1 0 1 typedval-lbl)]

		[(cn) (make-tagcase ;typedval-type-count
				    `(,gb-fail ,gb-call ,gb-fail)
				    #(0 0 0 0 0 0 1))]

		[(gb cn) (add-node gb cn)]

		[(gb) (car (foldl (lambda (node c)
				    (cons (add-edge (car c) node 1 cn (cdr c) typedval-lbl)
					  (+ 1 (cdr c))))
				  (cons gb 2) args))]
		[(gb) (~> gb
			  (add-edge op-res 1 cn 1 typedval-lbl))])#|
			  (add-edge op-res 1 tvl-elm 1 typedval-lbl)
			  (add-edge tvl-elm typedval-func-idx cls-elm 1 closure-lbl)
			  (add-edge bnd-lit 1 bnd-bld 1 int-lbl)
			  ;(add-edge cls-elm closure-framesize-idx bnd-bld 1 int-lbl)
			  (add-edge cls-elm closure-env-idx bck-bld back-frame-idx frame-lbl)
			  (add-edge bnd-bld 1 frm-bld frame-bind-idx typedval-array-lbl)
			  (add-edge bck-bld 1 frm-bld frame-prev-idx back-lbl)
			  (add-edge cls-elm closure-func-idx call 2 int-lbl)
			  (add-edge frm-bld 1 call 3 frame-lbl)
			  (add-edge cll-lit 1 call 1 call-function-lbl))])|#
    (values program gb cn)))
