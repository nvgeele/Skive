#!racket

(require "graph-boundary.rkt")

(provide compound-node?
	 make-tagcase
	 make-select
         make-for
	 compound-node-opcode
	 compound-node-subgraphs
	 compound-node-order)

(struct compound-node (opcode
		       subgraphs order)
	#:transparent)

(define opcode compound-node-opcode)
(define subgraphs compound-node-subgraphs)
(define order compound-node-order)

(define (subgraph-count cn)
  (length (subgraphs cn)))

;; Count is the amount of cases there are.
;; Boundaries is an *ordered* list of all the boundaries.
;; Order decides which graphs from ordered get executed
;; for which case or cases.
(define (make-tagcase boundaries order)
  (compound-node 2 boundaries order))

(define (make-select boundaries order)
  (compound-node 1 boundaries order))

(define (make-for generator body results)
  (compound-node 0 (list generator body results) #(0 1 2)))
