#!racket

(provide make-node node?
	 opcode
	 make-literal-node literal-node?
	 value)

(struct node (opcode)
	#:transparent)

(define make-node node)
(define opcode node-opcode)

(define (make-literal-node value)
  (node `(literal ,value)))

(define (literal-node? node)
  (and (list? (opcode node))
       (eq? 'literal (car (opcode node)))))

(define (value node)
  (cadr (opcode node)))
