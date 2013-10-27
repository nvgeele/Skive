#!racket

(provide make-node node?
	 opcode)

(struct node (opcode)
	#:transparent)

(define make-node node)
(define opcode node-opcode)
