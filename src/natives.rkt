#!racket

(require "node.rkt"
	 "typing.rkt"
	 (prefix-in cn- "compound-node.rkt"))

(provide natives
	 native-name
	 native-inputs
	 native-reducible?
	 native-neutral
	 native-type-lbl)

(struct native (name inputs reducible? neutral type-lbl))

(define natives
  (hash '+ (native "plus" 2 #t 0 binary-typedval-fun-lbl)
	'cons (native "cons" 2 #f '() binary-typedval-fun-lbl)
	'car (native "get_car" 1 #f '() unary-typedval-fun-lbl)
	'cdr (native "get_cdr" 1 #f '() unary-typedval-fun-lbl)))
