#!racket

(provide make-simple-node simple-node? opcode inputs chainable?
	 make-literal-node literal-node? value)

(struct node (type value inputs chainable?)
	#:transparent)

(define (make-simple-node opcode inputs [chainable? #f])
  (node 'simple opcode inputs chainable?))

(define (make-literal-node value)
  (node 'literal value 0 #f))

(define (literal-node? node)
  (eq? (node-type node) 'literal))

(define (simple-node? node)
  (eq? (node-type node) 'simple))

(define opcode node-value)
(define value node-value)

;; Is it chainable?
;; e.g.: native + is chainable
;; ->  (+ 5 5 5) := (+ 5 (+ 5 5))
(define chainable? node-chainable?)

;; Amount of input-ports available
(define inputs node-inputs)
