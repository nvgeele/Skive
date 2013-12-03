#!racket

(require "graph-boundary.rkt")

(provide compound-node?
	 make-tagcase)
	 #|make-compound-node
	 add-graph
	 subgraph-count|#

(struct compound-node (operation
		       subgraphs order)
	#:transparent)

(define operation compound-node-operation)
(define subgraphs compound-node-subgraphs)
(define order compound-node-order)

(define (make-compound-node)
  (compound-node '() 0 '()))

(define (add-graph cn graph [ord -1])
  (cond ((not (graph-boundary? graph))
	 (error "Not a graph boundary!"))
	((or (= -1 ord)
	     (<= 0 ord (subgraph-count cn)))
	 (struct-copy compound-node cn
		   [subgraphs (cons graph
				  (subgraphs cn))]
		   [order (if (= -1 ord)
			    (append order `(,(subgraph-count cn)))
			    (let-values (([a b] (split-at (order cn) ord)))
			      (append a `(,(subgraph-count cn)) b)))]))
	(else (error "Incorrect order"))))

(define (subgraph-count cn)
  (length (subgraphs cn)))

;; TODO: set-order

;; Count is the amount of cases there are.
;; Boundaries is an *ordered* list of all the boundaries.
;; Order decides which graphs from ordered get executed
;; for which case or cases.
(define (make-tagcase count boundaries order)
  (compound-node 2 boundaries order))
