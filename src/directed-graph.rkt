#!racket

(provide make-directed-graph
	 add-edge
	 node-data set-node-data
	 edge-data set-edge-data
	 for-each-node
	 for-each-edge)

;; Data in a graph struct is a list of cons cells.
;; The car of the cell is the data for the node,
;; the cdr is the list of connected edges.
;; This list of connected edges again is a list of
;; cons cells. Car is the `to' node, cdr is the 
;; data for this edge.

(struct directed-graph (data size))

(define data directed-graph-data)
(define size directed-graph-size)

(define (make-directed-graph)
  (directed-graph '() 0))

(define (add-edge graph from to (data '()))
  (let ((graph (if (> (max from to) (size graph))
		 (grow-graph graph (max from to))
		 graph)))
    (struct-copy directed-graph
		 [data (for/list ([i (build-list (size graph) values)]
				  [l (data graph)])
				 (if (= i from)
				   (cons (car l)
					 (cons (cons to data) (cdr l)))
				   l))])))

(define (node-data graph node)
  (if (<= node (size graph))
    (car (list-ref (data graph) node))
    #f))

(define (set-node-data graph node data)
  (if (<= node (size graph))
    (struct-copy directed-graph
		 [data (for/list ([i (build-list (size graph) values)]
				  [l (data graph)])
				 (if (= i node)
				   (cons data
					 (cdr l))
				   l))])
    #f)) ;; To error, or not to error, that is the question

(define (edge-data graph from to)
  (if (<= (max from to) (size graph))
    (let ((edges (cdr (list-ref (data graph) from))))
      (for/or ([edge edges])
	      (if (= (car edge) to)
		(cdr edge)
		#f)))
    #f))

;; If the edge doesn't exist, we aren't going
;; to create it!
(define (set-edge-data graph from to data)
  (if (<= (max from to) (size graph))
    (struct-copy directed-graph
		 [data (for/list ([i (build-list (size graph))]
				  [n (data graph)])
				 (if (= i from)
				   (cons (car n)
					 (for/list ([e (cdr n)])
						   (if (= to (car e))
						     (cons to data)
						     e)))
				   n))])
    #f))

;; Calls func for each node.
;; First argument is the data for that node,
;; second argument the list of edges.
(define (for-each-node graph func)
  (for ([node (data graph)])
       (func (car node) (cdr node))))

;; Calls func for each edge originating from
;; the node.
;; First argument is data for the edge,
;; second the to node.
(define (for-each-edge graph from func)
  (if (<= from (size graph))
    (let ((edges (cdr (list-ref (data graph) from))))
      (for ([edge edges])
	   (func (cdr edge) (car edge))))
    #f))

;;; Changes the size of the adjacency list
(define (grow-graph graph n)
  (let ((d (- n (size graph))))
    (unless (negative? d)
      (struct-copy directed-graph
		   [data (append (data graph)
				 (make-list d '(() ())))]
		   [size n]))))
