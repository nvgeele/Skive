#!racket

(require "node.rkt")
(require (prefix-in cn- "compound-node.rkt"))

(provide natives)

(define natives
  (hash '+ (make-node 141)
	'- (make-node 135)
	'* (make-node 152)
	'/ (make-node 122)))
