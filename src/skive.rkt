#!racket

(require "node.rkt"
	 "edge.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "ffi.rkt")

(provide compile
	 parse-and-graphviz
	 define-skive)

(define sisal-compiler-path "/usr/local/bin/sisalc")
(define gcc-path "/usr/bin/gcc")
(define runtime-object-path "/usr/local/lib/sisal/srt0.o")
(define sisal-lib-path "/usr/local/lib/sisal")
(define sisal-include-path "/usr/local/include/sisal")
(define graphviz-dot-path "/usr/local/bin/dot")

(define types
  "T 1 1 0 %na=Boolean\nT 2 1 1 %na=Character\nT 3 1 2 %na=Double\nT 4 1 3 %na=Integer\nT 5 1 4 %na=Null\nT 6 1 5 %na=Real\nT 7 1 6 %na=WildBasic\nT 8 10\nT 9 8 4 0\nT 10 3 0 9\n")

(define stamps
  "C$ D Nodes are DFOrdered\n")

(define-syntax define-skive
  (syntax-rules ()
    [(define-skive (name) . body)
     (define name
       (let loop ((gb (make-graph-boundary "main"))
		(cur (car (quote body)))
		(rem (cdr (quote body))))
       (let-values ([(new-gb result-node) (parse gb cur)])
	 (if (null? rem)
	   (let* ((transformed (transform-boundary new-gb result-node))
		  (path (compile-to-dylib
			  (string-append types stamps transformed))))
	     (make-thunk path))
	   (loop new-gb (car rem) (cdr rem))))))]))

;; Currently parses all into one boundary
;; => needs to be program when typing and
;; environments etc are implemented(?)
(define (compile code)
  (let*-values ([(gb result-node)
		 (parse (make-graph-boundary "main") code)]
		[(transformed)
		 (transform-boundary gb result-node)]
		[(path)
		 (compile-to-dylib
		   (string-append types stamps transformed))])
    (make-thunk path)))

(define (parse-and-graphviz code)
  (let*-values ([(dotfile)
		 (path->string (make-temporary-file "skivedot~a" #f "/tmp"))]
		[(output)
		 (open-output-file dotfile #:exists 'truncate)]
		[(pngfile)
		 (string-append dotfile ".png")]
		[(gb result-node)
		 (parse (make-graph-boundary "main") code)])
    (display (graph-boundary->dot-file gb) output)
    (close-output-port output)
    (let-values ([(sp out in err) (subprocess #f #f #f
					      graphviz-dot-path
					      "-Tpng" "-O" dotfile)])
      (subprocess-wait sp)(delete-file dotfile)
      (close-output-port in)(close-input-port out)(close-input-port err)
      (if (file-exists? pngfile)
	(display pngfile)
	(display "Could not create png file")))))

(define (compile-to-dylib code)
  (let* ((code-prefix (path->string (make-temporary-file "skiveif1~a" #f "/tmp")))
	 (code-file (string-append code-prefix ".if1"))
	 (csrc-file (string-append code-prefix ".c"))
	 (cobj-file (string-append code-prefix ".o"))
	 (clib-file (string-append code-prefix ".dylib"))
	 (out (open-output-file code-file #:exists 'truncate)))
    (display code out)
    (close-output-port out)
    (let-values ([(sp out in err) (subprocess #f #f #f
					      sisal-compiler-path 
					      "-C" code-file)])
      (subprocess-wait sp)
      (close-output-port in)(close-input-port out)(close-input-port err)
      (if (file-exists? csrc-file)
	(let-values ([(sp out in err) (subprocess #f #f #f
						  gcc-path csrc-file
						  "-c" "-o" cobj-file
						  (~a "-I" sisal-include-path)
						  "-g" "-O2")])
	  (subprocess-wait sp)
	  (close-output-port in)(close-input-port out)(close-input-port err)
	  (if (file-exists? cobj-file)
	    (let-values ([(sp out in err) (subprocess #f #f #f
						      gcc-path
						      "-o" clib-file
						      runtime-object-path
						      cobj-file
						      (~a "-L" sisal-lib-path)
						      "-lsisal" "-lm")])
	      (subprocess-wait sp)
	      (close-output-port in)(close-input-port out)(close-input-port err)
	      (delete-file code-file)(delete-file csrc-file)
	      (delete-file cobj-file)(delete-file code-prefix)
	      (if (file-exists? clib-file)
		clib-file
		(error "Could not compile dylib")))
	    (error "Could not compile C source to object file")))
	(error "Could not create C source file.")))))

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
		    (reducible? native)))
	   (error "Incorrect amount of arguments for function!"))
	  ((and (> (length operands) (inputs native))
		(reducible? native))
	   (parse-application boundary (reduce exp)))
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

(define (reduce exp)
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
