#!racket

(require "edge.rkt"
	 "node.rkt")

(provide make-graph-boundary
	 add-node add-edge
	 graph-boundary?
	 for-each-node
	 for-each-edge)

(struct graph-boundary (;type
			name
			label-counter
			nodes edges
			global)
	#:transparent)

(define name graph-boundary-name)
(define label-counter graph-boundary-label-counter)
(define nodes graph-boundary-nodes)
(define edges graph-boundary-edges)

(define (make-graph-boundary ;type
			     name [global #f])
  (graph-boundary ;type
		  name 1 (hash) '() global))

(define (add-node gb node)
  (values
    (struct-copy graph-boundary gb
		 [label-counter (+ 1 (label-counter gb))]
		 [nodes (hash-set (nodes gb)
				  (label-counter gb)
				  node)])
    (label-counter gb)))

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
	  ((not (and (port-free? gb in-id in-port)
		     (port-free? gb out-id out-port)))
	   (error "One of the ports is already connected!"))
	  (else
	    (struct-copy graph-boundary gb
			 [edges (cons (make-edge in-id in-port
						 out-id out-port)
				      edges)])))))
;; proc: label node -> any
(define (for-each-node gb proc)
  (for ([(label node) (nodes gb)])
    (proc label node)))

;; proc: edge -> any
(define (for-each-edge gb node proc)
  (for ([edge (filter (lambda (edge)
			(= node (edge-in-node edge)))
		   (edges gb))])
    (proc edge)))

;; proc: label node any -> any
(define (foldl-nodes gb init proc)
  (foldl (lambda (p res) (proc (car p) (cdr p) res))
	 init
	 (hash->list (nodes gb)))

;; proc: edge any -> any
(define (foldl-edges gb node init proc)
  (foldl proc init
	 (filter (lambda (edge)
		   (= node (edge-in-node edge)))
		 (edges gb))))
