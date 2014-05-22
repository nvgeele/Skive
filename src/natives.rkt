#!racket

(require "node.rkt"
	 "typing.rkt"
	 (prefix-in cn- "compound-node.rkt"))

(provide natives
	 natives-list
	 native-name
	 native-inputs
	 native-reducible?
	 native-neutral)

(struct native (name inputs reducible? neutral))

(define natives
  (hash '+ (native "plus" 2 #t 0)
	'- (native "minus" 2 #t 0)
	'* (native "multiply" 2 #t 1)
	'/ (native "divide" 2 #f 1)
	'=   (native "equal" 2 #f '())
	'cons (native "cons" 2 #f '())
	'car (native "get_car" 1 #f '())
	'cdr (native "get_cdr" 1 #f '())
        'list? (native "is_list" 2 #f '())
        'apply (native "apply" 2 #f '())
        'map-vector (native "map_vector" 2 #f '())
        'vector-ref (native "vector_ref" 2 #f '())
        'vector-length (native "vector_length" 1 #f '())
        'make-vector (native "make_vector" 2 #f '())
        'error (native "throw_error" 0 #f '())))

;; Some steps in the compilation process need a list of natives.
;; Hash-map makes a list but does this in undefined order! Since
;; the order of the list needs to be the same in every step, we
;; generate the list here, only once, so all steps share the
;; same order.

(define natives-list
  (hash->list natives))
