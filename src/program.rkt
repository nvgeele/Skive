#!racket

(require "graph-boundary.rkt")

(provide make-program program?
	 add-graph-boundary)

(struct program (boundaries)
	#:transparent)

(define (make-program)
  (program '()))


(define (add-graph-boundary p gb)
  (if (graph-boundary? gb)
    (struct-copy program p
		 [boundaries (cons gb
				   (boundaries p))])
    (error "Not a graph-boundary!")))
