#!racket

(require "threading.rkt"
	 "syntax.rkt"
	 "node.rkt"
	 "edge.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "typing.rkt"
	 "program.rkt")

(provide generate)

(define (generate exp)
  (let ((program (make-program))
	(boundary (make-graph-boundary "entry")))
    (generate* exp boundary program)))

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
	 exp)
	(else (error "Incorrect expression -- generate"))))

(define (generate-lookup graph-boundary frame offset)
  (let loop ((frame frame)
	     (gb graph-boundary)
	     (res 0))
    (if (= frame 0)
      (let*-values ([(gb res1) (add-node gb (make-simple-node 144))]  ;; RElements
		    [(gb res2) (add-node gb (make-simple-node 105))]) ;; AElement
	(values (add-edge (add-edge gb res 1 ;; Assuming frame-idx is always 1!
				    res1 1 frame-lbl)
			  res1 frame-bind-idx
			  res2 1 typedval-array-lbl)
		res2))
      (let*-values ([(gb res1) (add-node gb (make-simple-node 144))]  ;; RElements
		    [(gb res2) (add-node gb (make-simple-node 144))]) ;; RElements
	(loop (- frame 1)
	      (add-edge (add-edge gb res 1 ;; Assuming frame-idx is always 1!
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
  (let*-values ([(label) (next-label program)]
		[(boundary) (make-graph-boundary label)]
		[(program gb res) (generate-sequence* (lambda-body exp) boundary program)]
		[(gb) (add-edge gb res 1 0 1 typedval-lbl)]
		[(program) (add-boundary program gb)]
		[(gb lit1) (add-node graph-boundary (make-literal-node label))]
		[(gb lit2) (add-node gb (make-literal-node (length (lambda-args exp))))]
		[(gb bld1) (add-node gb (make-simple-node 143))]
		[(gb bld2) (add-node gb (make-simple-node 143))])
    (values program
	    (~> gb
		(add-edge lit1 1 bld1 closure-func-idx function-lbl)
		(add-edge lit2 1 bld1 closure-args-idx int-lbl)
		(add-edge lit2 1 bld1 closure-framesize-idx int-lbl)
		(add-edge 0    1 bld1 closure-env-idx frame-lbl)
		(add-edge bld1 1 bld2 typedval-func-idx closure-lbl))
	    bld2)))
