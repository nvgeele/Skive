#!racket

(require "node.rkt")
(require (prefix-in cn- "compound-node.rkt"))

(provide natives)

(define natives
  (hash '+ (make-simple-node 141)
	'- (make-simple-node 135)
	'* (make-simple-node 152)
	'/ (make-simple-node 122)))
