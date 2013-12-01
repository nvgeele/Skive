#!racket

(require "syntax.rkt"
	 "node.rkt"
	 "edge.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "typing.rkt"
	 "program.rkt")

(provide generate)

(define (generate exp)
  (generate* exp
	     (make-graph-boundary "entry")
	     '()))

(define (generate* exp graph-boundary boundaries)
  (cond ((lexical-address? exp)
	 (let-values ([(gb res) (generate-lookup graph-boundary
						 (vector-ref exp 0)
						 (vector-ref exp 1))])
	   (values (cons gb boundaries)
		   res)))
	((self-evaluating? exp)
	 (generate-self-evaluating graph-boundary exp))
	((lambda? exp)
	 exp)
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
	(display (~a "Z: " gb "\n" res " - " res1 " - " res2 "\n\n"))
	(values (add-edge (add-edge gb res 1 ;; Assuming frame-idx is always 1!
				    res1 1 frame-lbl)
			  res1 frame-bind-idx
			  res2 1 typedval-array-lbl)
		res2))
      (let*-values ([(gb res1) (add-node gb (make-simple-node 144))]  ;; RElements
		    [(gb res2) (add-node gb (make-simple-node 144))]) ;; RElements
	(display (~a "NZ: " gb "\n" res " - " res1 " - " res2 "\n\n"))
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
