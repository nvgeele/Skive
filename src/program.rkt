#!racket

(require "natives.rkt")
(require "graph-boundary.rkt")
(require "node.rkt")
(require "edge.rkt")

(provide make-program
	 add-boundary
	 next-label next-label-str
	 program-boundaries
	 program-count
	 program->dot-file)

(struct program (count boundaries)
	#:transparent)

(define (make-program)
  (program (hash-count natives) '()))

(define (next-label-str program)
  (~a "proc" (program-count program)))

(define (next-label program)
  (program-count program))

(define (add-boundary prog boundary)
  (program (+ (program-count prog) 1)
	   (cons boundary (program-boundaries prog))))

(define (program->dot-file program)
  (let loop ((cur (car (program-boundaries program)))
	     (rem (cdr (program-boundaries program)))
	     (res "digraph G {\n")
	     (offset 0))
    (let ((res (string-append
		 res
		 (foldl-nodes cur (~a "subgraph \"" (graph-boundary-name cur) "\" {\n")
			      (lambda (label node res)
				(let ((node-label (~a "node" (+ label offset)))
				      (node-text
					(~a "node " label "\\n"
					    (cond ((literal-node? node)
						   (~a "literal: "
						       (value node)))
						  ((simple-node? node)
						   (~a "simple: "
						       (opcode node)))
						  (else "compound node")))))
				  (~a res
				      (foldl-edges cur label
						   (~a node-label " [label=\""
						       node-text "\"];\n")
						   (lambda (edge res)
						     (~a res
							 node-label
							 " -> node" (+ (edge-out-node edge) offset)
							 " [label=\"" (edge-out-port edge) "\"];\n")))))))
		 "}\n")))
      (if (null? rem)
	(string-append res "}")
	(loop (car rem) (cdr rem) res (+ offset (label-counter cur)))))))
