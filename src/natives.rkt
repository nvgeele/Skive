#!racket

(require "node.rkt")
(require (prefix-in cn- "compound-node.rkt"))

(provide natives)

(define natives
  (hash '+ (make-simple-node 141 2 #t)
	'- (make-simple-node 135 2 #t)
	'* (make-simple-node 152 2 #t)
	'/ (make-simple-node 122 2 #t)))
