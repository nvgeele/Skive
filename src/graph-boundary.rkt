#!racket

(require "edge.rkt"
	 "node.rkt")

(provide make-graph-boundary
	 add-node add-edge
	 graph-boundary?
	 for-each-node
	 for-each-edge
	 foldl-nodes
	 foldl-edges
	 foldl-edges-to
	 graph-boundary->dot-file
	 label-counter
	 graph-boundary-name
	 topological-sort
	 graph-node
	 set-boundary-name)

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
  (not (ormap (lambda (edge) (and (= node (edge-out-node edge))
				  (= port (edge-out-port edge))))
	      (edges gb))))

(define (add-edge gb in-id in-port out-id out-port type-lbl)
  (let ((nodes (nodes gb))
	(edges (edges gb)))
    (cond ((or (and (null? (hash-ref nodes in-id null))
		    (not (= in-id 0)))
	       (and (null? (hash-ref nodes out-id null))
		    (not (= out-id 0))))
	   (error "One or more of the node id's are invalid -- add-edge"))
	  ((not (port-free? gb out-id out-port))
	   (error "One of the ports is already connected -- add-edge"))
	  (else
           (struct-copy graph-boundary gb
                        [edges (cons (if type-lbl
                                         (make-edge in-id in-port
                                                    out-id out-port
                                                    type-lbl)
                                         (make-edge in-id in-port
                                                    out-id out-port))
                                     edges)])))))

(define (set-boundary-name boundary new-name)
  (struct-copy graph-boundary boundary
	       [name new-name]))

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
	 (hash->list (nodes gb))))

;; proc: edge any -> any
(define (foldl-edges gb node init proc)
  (foldl proc init
	 (filter (lambda (edge)
		   (= node (edge-in-node edge)))
		 (edges gb))))

;; proc: edge any -> any
(define (foldl-edges-to gb node init proc)
  (foldl proc init
	 (filter (lambda (edge)
		   (= node (edge-out-node edge)))
		 (edges gb))))

(define (graph-boundary->dot-file gb)
  (~a (foldl-nodes gb (~a "digraph \"" (name gb) "\" { ")
		   (lambda (label node res)
		     (let ((node-label (~a "node" label))
			   (node-text
                            (~a "node " label "\\n"
                                (cond ((literal-node? node)
                                       (~a "literal: "
                                           (value node)))
                                      ((simple-node? node)
                                       (~a "simple: "
                                           (opcode node)))
                                      (else "compound node")))))
		       (~a res
			   (foldl-edges gb label
					(~a node-label " [label=\""
					    node-text "\"];")
					(lambda (edge res)
					  (~a res
					      node-label
					      " -> node" (edge-out-node edge)
					      " [label=\""
					      (edge-out-port edge) "\\n"
					      (edge-type-lbl edge) "\"];")))))))
      "}"))

(define (topological-sort gb)
  (define (dft-rec from stack visited)
    (let ((edges (map edge-out-node (filter (lambda (edge)
					      (= from (edge-in-node edge)))
					    (edges gb)))))
      (let* ((cell (foldl (lambda (to c)
			    (if (set-member? (car c) to)
                                c
                                (dft-rec to (cdr c) (set-add (car c) to))))
			  (cons visited stack)
			  edges))
	     (stack (cdr cell))
	     (visited (car cell)))
	(cons visited
	      (cons from stack)))))
  (cdr (foldl (lambda (from cell)
		(if (set-member? (car cell) from)
                    cell
                    (dft-rec from (cdr cell) (set-add (car cell) from))))
	      (cons (set) '())
	      (map car (hash->list (nodes gb))))))

(define (graph-node boundary id)
  (hash-ref (nodes boundary) id #f))
