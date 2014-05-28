#!racket

(require "node.rkt"
	 "typing.rkt"
	 (prefix-in cn- "compound-node.rkt"))

(provide natives
	 natives-list
	 native-name
	 native-inputs)

(struct native (name inputs))

(define natives
  (hash '+ (native "plus" 2)
	'- (native "minus" 2)
	'* (native "multiply" 2)
	'/ (native "divide" 2)
	'=   (native "equal" 2)
	'cons (native "cons" 2)
	'car (native "get_car" 1)
	'cdr (native "get_cdr" 1)
        'list? (native "is_list" 2)
        'apply (native "apply" 2)
        'map-vector (native "map_vector" 2)
        'vector-ref (native "vector_ref" 2)
        'vector-set (native "vector_set" 3)
        'vector-length (native "vector_length" 1)
        'make-vector (native "make_vector" 2)
        'error (native "throw_error" 0)
        'null? (native "is_null" 1)
        'false? (native "is_false" 1)
        'not (native "is_false" 1)))

;; Some steps in the compilation process need a list of natives.
;; Hash-map makes a list but does this in undefined order! Since
;; the order of the list needs to be the same in every step, we
;; generate the list here, only once, so all steps share the
;; same order.

(define natives-list
  (hash->list natives))
