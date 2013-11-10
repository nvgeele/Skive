#!racket

(require "typing.rkt")

(provide edge?
	 make-edge
	 edge-in-node
	 edge-in-port
	 edge-out-node
	 edge-out-port
	 edge-type-lbl)

(struct edge (in-node in-port out-node out-port type-lbl)
	#:transparent)

(define (make-edge in-node in-port
		   out-node out-port
		   [type-lbl typedval-lbl])
  (edge in-node in-port
	out-node out-port
	type-lbl))
