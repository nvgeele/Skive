#!racket

(provide make
	 connected?
	 add-edge
	 node-data set-node-data
	 edge-data set-edge-data
	 for-each-node
	 for-each-edge)

;; This isn't a "real" graph; a real mathematical
;; graph has a finite set of nodes/vertices,
;; whilst this representation allows for a
;; growing amount of vertices.

;; Data in a graph struct is a hash.
;; For each node there is a cons cell assigned as
;; value. The car of this cons cell is the data
;; for this node, the cdr is the hash of outgoing
;; edges.

;; The hash of outgoing edges has a value assigned
;; to each node that is connected with the node.
;; This is the data for this edge. The value can
;; be empty

(struct directed-graph (data)
	#:transparent)

(define data directed-graph-data)

(define (make)
  (directed-graph (hash)))

(define (connected? graph from to)
  (let ((edges (hash-ref (data graph) from #f)))
    (if edges
      (not (false? (hash-ref (cdr edges) to #f)))
      #f)))

;; Will add an edge, as long as from and to aren't
;; connected yet. If a node has no data yet, then
;; it well just be initialized with empty data.
(define (add-edge graph from to (new-data '()))
  (if (connected? graph from to)
    #f ;; To error or not to error, that is the question
    (directed-graph (hash-set (data graph) from
			      (let ((node (hash-ref (data graph)
						    from (cons '() (hash)))))
				(cons (car node)
				      (hash-set (cdr node) to new-data)))))))

(define (node-data graph node)
  (let ((data (hash-ref (data graph) node #f)))
    (if data (car data) #f)))

(define (set-node-data graph node new-data)
  (let ((edges (cdr (hash-ref (data graph) node (cons '() (hash))))))
    (directed-graph (hash-set (data graph) node
			      (cons new-data edges)))))

(define (edge-data graph from to)
  (if (connected? from to)
    (let ((edges (cdr (hash-ref (data graph) from))))
      (hash-ref edges to))
    #f))

;; If the edge doesn't exist, we aren't going
;; to create it!
(define (set-edge-data graph from to new-data)
  (if (connected? from to)
    (let ((node (hash-ref (data graph) from)))
      (directed-graph (hash-set (data graph) from
				(cons (car node)
				      (hash-set (cdr node) to new-data)))))
    #f))

;; Calls func for each node.
;; First argument is the data for that node,
;; second argument the list of edges.
(define (for-each-node graph func)
  (for ([(n v) (data graph)])
       (func (car v) (cdr v))))

;; Calls func for each edge originating from
;; the node.
;; First argument is data for the edge,
;; second the to node.
(define (for-each-edge graph from func)
  (let ((edges (cdr (hash-ref (data graph) from (cons '() (hash))))))
    (for ([(to data) edges])
	 (func data to))))
