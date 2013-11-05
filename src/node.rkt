#!racket

(provide make-simple-node simple-node? opcode inputs reducible?
	 make-literal-node literal-node? value)

(struct node (type value inputs reducible?)
	#:transparent)

(define (make-simple-node opcode inputs [reducible? #f])
  (node 'simple opcode inputs reducible?))

(define (make-literal-node value)
  (node 'literal value 0 #f))

(define (literal-node? node)
  (eq? (node-type node) 'literal))

(define (simple-node? node)
  (eq? (node-type node) 'simple))

(define opcode node-value)
(define value node-value)

;; Can the expression be reduced?
;; e.g.: native + is reducible
;; ->  (+ 5 5 5) := (+ 5 (+ 5 5))
(define (reducible? node)
  (and (node-reducible? node)
       (= 2 (node-inputs node))))

;; Amount of input-ports available
(define inputs node-inputs)
