#!racket

(require "node.rkt"
	 "typing.rkt")

(require (prefix-in cn- "compound-node.rkt"))

(provide natives
	 native-name
	 native-inputs
	 native-reducible?
	 native-type-lbl)

(struct native (name inputs reducible? type-lbl))

(define natives
  (hash '+ (native "plus" 2 #t binary-typedval-fun-lbl)
	'cons (native "cons" 2 #f binary-typedval-fun-lbl)
	'car (native "get_car" 1 #f unary-typedval-fun-lbl)
	'cdr (native "get_cdr" 1 #f unary-typedval-fun-lbl)))
