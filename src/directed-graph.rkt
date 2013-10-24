#!racket

(provide make-directed-graph
	 add-edge!
	 node-data node-data!
	 edge-data edge-data!
	 for-each-node
	 for-each-edge)

(struct directed-graph (data current-size))

(define (make-directed-graph)
  (directed-graph '() 0))

(define (add-edge! graph from to (data '()))
  '())

(define (node-data graph node)
  '())

(define (node-data! graph node)
  '())

(define (edge-data graph from to)
  '())

(define (edge-data! graph from to)
  '())

(define (grow-graph! graph n)
  '())

(define (for-each-node graph func)
  '())

(define (for-each-edge graph from func)
  '())
