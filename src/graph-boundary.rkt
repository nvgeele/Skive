#!racket

(require ;(prefix-in dg- "directed-graph.rkt")
	 "edge.rkt"
	 "node.rkt"
	 "compound-node.rkt")

(struct graph-boundary (;type
			name ;graph
			label-counter [global #f]
			nodes edges)
	#:transparent)

;(define type graph-boundary-type)
(define name graph-boundary-name)
;(define graph graph-boundary-graph)
(define label-counter graph-boundary-label-counter)
(define nodes graph-boundary-nodes)
(define edges graph-boundary-edges)

(define (make type name [global #f])
  (graph-boundary type name (dg-new) 1 global (hash) '()))

(define (add-node gb node)
  (if (or (node? node)
	  (compound-node? node))
    (struct-copy graph-boundary gb
		 [label-counter (+ 1 (label-counter gb))]
		 [nodes (hash-set (nodes gb)
				  (label-counter gb)
				  node)])
    (error "Argument is not a valid node!")))

(define (port-free? gb node port)
  (not (ormap (lambda (edge) (and (= node (edge-in-node edge))
				  (= port (edge-in-port edge))))
	      (edges gb))))

(define (add-edge gb in-id in-port out-id out-port)
  (let ((nodes (nodes gb))
	(edges (edges gb)))
    (cond ((or (null? (hash-ref nodes in-id null))
	      (null? (hash-ref nodes out-id null)))
	   (error "One or more of the node id's are invalid!"))
	  ((and (port-free? gb in-id in-port)
		(port-free? gb out-id out-port))
	   (error "One of the ports is already connected!"))
	  (else
	    (struct-copy graph-boundary gb
			 [edges (cons (make-edge in-id in-port
						 out-id out-port)
				      edges)])))))
