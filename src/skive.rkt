#!racket

(require "node.rkt"
	 "edge.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt")

(provide compile
	 parse-and-graphviz)

(define compiler-path "/usr/local/bin/sisalc")

(define types
  "T 1 1 0 %na=Boolean\nT 2 1 1 %na=Character\nT 3 1 2 %na=Double\nT 4 1 3 %na=Integer\nT 5 1 4 %na=Null\nT 6 1 5 %na=Real\nT 7 1 6 %na=WildBasic\nT 8 10\nT 9 8 4 0\nT 10 3 0 9\n")

(define stamps
  "C$ D Nodes are DFOrdered\n")

;; Currently parses all into one boundary
;; => needs to be program when typing and
;; environments etc are implemented(?)

(define (compile code)
  (let*-values ([(gb result-node)
		 (parse (make-graph-boundary "main") code)]
		[(transformed)
		 (transform-boundary gb result-node)]
		[(path)
		 (compile-to-native
		   (string-append types stamps transformed))])
    (create-wrapper path)))

(define (parse-and-graphviz code)
  (let*-values ([(dotfile)
		 (make-temporary-file "skivedot~a.dot" #f "/tmp")]
		[(output)
		 (open-output-file dotfile #:exists 'truncate)]
		[(gb result-node)
		 (parse (make-graph-boundary "main") code)])
    (display (~a (foldl-nodes gb "digraph G { "
			      (lambda (label node res)
				(let ((node-label (~a "node" label))
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
				      (foldl-edges gb label
						   (~a node-label " [label=\""
						       node-text "\"];")
						   (lambda (edge res)
						     (~a res
							 node-label
							 " -> node" (edge-out-node edge)
							 " [label=\"" (edge-out-port edge) "\"];")))))))
		 "}") output)
    (close-output-port output)
    (display "Generated dot-file: ")(display dotfile)))


(define (create-wrapper path)
  (lambda ()
    (let-values ([(sp out in err)
		  (subprocess #f #f #f
			      path)])
      (subprocess-wait sp)
      (let ((output (read out)))
	(close-output-port in)
	(close-input-port out)
	(close-input-port err)
	output))))

(define (compile-to-native code)
  (let* ((code-file (make-temporary-file "skiveif1~a.if1" #f "/tmp"))
	 (exec-file (make-temporary-file "skiveexe~a" #f "/tmp"))
	 (out (open-output-file code-file #:exists 'truncate)))
    (display code out)
    (close-output-port out)
    (let-values ([(sp out in err) (subprocess #f #f #f
					      compiler-path 
				 	      "-o" exec-file
					      code-file)])
      (subprocess-wait sp)
      (close-output-port in)
      (close-input-port out)
      (close-input-port err))
    exec-file))

(define (parse boundary exp); linkage)
  (cond ((self-evaluating? exp)
	 (parse-self-evaluating boundary exp))
	((application? exp)
	 (parse-application boundary exp)); linkage))
	(else (error "Incorrect expression"))))

(define (self-evaluating? exp)
  (or (integer? exp)))

(define (parse-self-evaluating boundary exp)
  (let ((node (make-literal-node exp)))
    (let-values ([(boundary label) (add-node boundary node)])
      (values boundary label))))

(define (application? exp)
  (list? exp))

(define (parse-application boundary exp); linkage)
  (let* ((operator (car exp))
	 (operands (cdr exp))
	 (native (hash-ref natives operator #f)))
    (cond ((not native) (error "Only native functions supported!"))
	  ((not (or (= (length operands) (inputs native))
		    (chainable? native)))
	   (error "Incorrect amount of arguments for function!"))
	  ((and (> (length operands) (inputs native))
		(chainable? native))
	   (parse-application boundary (chain exp)))
	  (else (let*-values ([(boundary inputs)
			       (parse-operands boundary operands)]
			      [(boundary oplabel)
			       (add-node boundary native)])
		  (values (car (foldl (lambda (input boundary)
					(cons (add-edge (car boundary)
							input 1
							oplabel (cdr boundary))
					      (+ 1 (cdr boundary))))
				      (cons boundary 1)
				      inputs))
			  oplabel))))))

(define (chain exp)
  (let ((op (car exp)))
    (let loop ((args (cdr exp))
	       (np2 (cdddr exp)))
      (if (null? np2)
	`(,op ,(car args) ,(cadr args))
	`(,op ,(car args)
	      ,(loop (cdr args)
		     (cdddr args)))))))

;; Accepts a graph-boundary and a list of operands.
;; Returns boundary with nodes/edges of operands added
;; and a list of the operands' node labels.
(define (parse-operands boundary operands)
  (if (null? operands)
    (values boundary '())
    (let loop ((boundary boundary)
	       (inputs '())
	       (cur (car operands))
	       (rem (cdr operands)))
      (let-values ([(gb link) (parse boundary cur)])
	(if (null? rem)
	  (values gb (reverse (cons link inputs)))
	  (loop gb (cons link inputs) (car rem) (cdr rem)))))))

;; Transforms abstract graph-boundary to IF1 code.
(define (transform-boundary boundary result-node)
  (~a
    (foldl-nodes boundary "X 10 \"main\"\n"
		 (lambda (label node res)
		   (string-append
		     res
		     (cond
		       ((literal-node? node) (transform-literal-node boundary node label))
		       ((simple-node? node) (transform-simple-node boundary node label))
		       (else (transform-compound-node boundary node label))))))
    "E " result-node " 1 0 1 4\n"))

(define (transform-simple-node boundary node label)
  (let ((opcode (opcode node)))
    (foldl-edges boundary label
		 (~a "N " label " " opcode "\n")
		 (lambda (edge res)
		   (string-append
		     res
		     (~a "E " label
			 " " (edge-in-port edge)
			 " " (edge-out-node edge)
			 " " (edge-out-port edge)
			 " 4\n"))))))


(define (transform-literal-node boundary node label)
  (let ((value (value node)))
    (foldl-edges boundary label
		 "" (lambda (edge res)
		      (~a "L " (edge-out-node edge)
			  " "  (edge-out-port edge)
			  " 4 \"" value "\"\n")))))

(define (transform-compound-node boundary label)
  "")
