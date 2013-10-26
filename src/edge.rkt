#!racket

(provide edge?
	 make-edge
	 edge-in-node
	 edge-in-port
	 edge-out-node
	 edge-out-port)

(struct edge (in-node in-port out-node out-port)
	#:transparent)

(define (make-edge in-node in-port
		   out-node out-port)
  (edge in-node in-port
	out-node out-port))
