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
         program-intern-symbol
	 program->dot-file)

(struct symbol-table (count table)
        #:transparent)

(struct program (count boundaries symbol-table)
	#:transparent)

(define (make-program)
  (program (hash-count natives) '() (symbol-table 0 (hash))))

(define (next-label-str program)
  (~a "proc_" (program-count program)))

(define (next-label program)
  (program-count program))

(define (add-boundary prog boundary)
  (program (+ (program-count prog) 1)
	   (cons boundary (program-boundaries prog))
           (program-symbol-table prog)))

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

(define (program-intern-symbol prog symbol)
  (let* ((table (program-symbol-table prog))
         (interned (hash-ref (symbol-table-table table) symbol #f)))
    (if interned
        (values prog interned)
        (let ((interned (symbol-table-count table)))
          (values
           (struct-copy program prog
                        [symbol-table (symbol-table
                                       (+ (symbol-table-count table) 1)
                                       (hash-set (symbol-table-table table)
                                                 symbol
                                                 interned))])
           interned)))))
