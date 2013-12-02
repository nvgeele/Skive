#!racket

(provide make-simple-node simple-node? opcode
	 make-literal-node literal-node? value)

(struct node (type value)
	#:transparent)

(define (make-simple-node opcode)
  (node 'simple opcode))

(define (make-literal-node value)
  (node 'literal value))

(define (literal-node? node)
  (and (node? node)
       (eq? (node-type node) 'literal)))

(define (simple-node? node)
  (and (node? node)
       (eq? (node-type node) 'simple)))

(define opcode node-value)
(define value node-value)
