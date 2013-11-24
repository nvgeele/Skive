#!racket

(require "syntax.rkt"
	 "expand.rkt"
	 "node.rkt"
	 "edge.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "ffi.rkt"
	 "typing.rkt"
	 "runtime.rkt")

(provide compile
	 parse-and-graphviz
	 define-skive)

(define sisal-compiler-path "/usr/local/bin/sisalc")
(define gcc-path "/usr/bin/gcc")
(define runtime-object-path "/usr/local/lib/sisal/srt0.o")
(define sisal-lib-path "/usr/local/lib/sisal")
(define sisal-include-path "/usr/local/include/sisal")
(define graphviz-dot-path "/usr/local/bin/dot")

(define stamps
  "C$ D Nodes are DFOrdered\n")

(define-syntax define-skive
  (syntax-rules ()
    [(define-skive (name) . body)
     (define name
       (let* ((code (compile-if1 (quote body)))
	      (lib (compile-native code #:type 'lib)))
	 (make-thunk lib)))]))

;; Currently parses all into one boundary
;; => needs to be program when typing and
;; environments etc are implemented(?)
;; !!! accepts a sequence of expressions !!!
(define (compile-if1 code)
  (let*-values ([(expanded)
		 (map expand code)]
		[(gb result-node)
		 (let ((res (foldl (lambda (exp cell)
				     (let-values ([(gb res)
						   (parse-boundary
						     (car cell)
						     exp)])
				       (cons gb res)))
				   (cons (make-graph-boundary "main") 0)
				   expanded)))
		   (values (car res) (cdr res)))]
		[(transformed)
		 (transform-boundary gb result-node)]
		[(code)
		 (string-append (generate-type-definitions-code)
				stamps
				(generate-runtime-code)
				transformed)])
    code))

(define (parse-and-graphviz code)
  (let*-values ([(pngfile)
		 (path->string (make-temporary-file "skivedot~a.png" #f "/tmp"))]
		[(file-out)
		 (open-output-file pngfile #:exists 'truncate)]
		[(gb result-node)
		 (parse-boundary (make-graph-boundary "main") code)]
		[(dotcode)
		 (graph-boundary->dot-file gb)])
    (let-values ([(sp out in err)
		  (subprocess #f #f #f
			      graphviz-dot-path
			      "-Tpng")])
      (display dotcode in)
      (flush-output in)
      (close-output-port in)
      (subprocess-wait sp)
      (copy-port out file-out)
      (flush-output file-out)
      (close-input-port err)
      (close-input-port out)
      pngfile)))

;; TODO: add additional paramter to supply output path
(define (compile-native code #:type [type 'exe])
  (let* ((code-prefix (path->string (make-temporary-file "skiveif1~a" #f "/tmp")))
	 (code-file (string-append code-prefix ".if1"))
	 (csrc-file (string-append code-prefix ".c"))
	 (cobj-file (string-append code-prefix ".o"))
	 (out-file (string-append code-prefix
				  (case type
				    [(exe) ""]
				    [(lib) ".dylib"]
				    [else (error "Incorrect output type -- compile-native")])))
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
						      "-o" out-file
						      runtime-object-path
						      cobj-file
						      (if (eq? type 'lib)
							"-dynamiclib" "")
						      (~a "-L" sisal-lib-path)
						      "-lsisal" "-lm")])
	      (subprocess-wait sp)
	      (close-output-port in)(close-input-port out)(close-input-port err)
	      (delete-file code-file)(delete-file csrc-file)
	      (delete-file cobj-file)(delete-file code-prefix)
	      (if (file-exists? out-file)
		out-file
		(error "Could not compile output file -- compile-native")))
	    (error "Could not compile C source to object file -- compile-native")))
	(error "Could not create C source file -- compile-native")))))

(define (parse-boundary gb exp)
  (cond ((self-evaluating? exp)
	 (parse-self-evaluating gb exp))
	((application? exp)
	 (parse-application gb exp))
	(else (error "Incorrect expression -- parse-boundary"))))

(define (parse-self-evaluating boundary exp)
  (let*-values ([(lit-node build-node) (values (make-literal-node exp)
					       (make-simple-node 143))]
		[(type-lbl type-idx) (cond ((integer? exp) (values int-lbl
								   typedval-int-idx))
					   ((string? exp) (values string-lbl
								  typedval-string-idx))
					   (else (error "Unknown error")))]
		[(gb blbl) (add-node boundary build-node)]
		[(gb llbl) (add-node gb lit-node)]
		[(gb) (add-edge gb llbl 1 blbl type-idx type-lbl)])
    (values gb blbl)))

(define (parse-application boundary exp)
  (let* ((operator (car exp))
	 (operands (cdr exp))
	 (native (hash-ref natives operator #f)))
    (cond ((not native) (error "Only native functions supported!"))
	  ((not (or (= (length operands) (native-inputs native))
		    (native-reducible? native)))
	   (error "Incorrect amount of arguments for function!"))
	  ((and (> (length operands) (native-inputs native))
		(native-reducible? native))
	   (parse-application boundary (reduce exp)))
	  (else (let*-values
		  ([(gb inputs) (parse-operands boundary operands)]
		   [(gb call-lbl) (add-node gb (make-simple-node 120))]
		   [(gb litt-lbl) (add-node gb (make-literal-node (native-name native)))]
		   [(gb) (add-edge gb litt-lbl 1 call-lbl 1 (native-type-lbl native))])
		  (values (car (foldl (lambda (input boundary)
					(cons (add-edge (car boundary)
							input 1
							call-lbl (cdr boundary)
							typedval-lbl)
					      (+ 1 (cdr boundary))))
				      (cons gb 2)
				      inputs))
			  call-lbl))))))

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
      (let-values ([(gb link) (parse-boundary boundary cur)])
	(if (null? rem)
	  (values gb (reverse (cons link inputs)))
	  (loop gb (cons link inputs) (car rem) (cdr rem)))))))

;; Transforms abstract graph-boundary to IF1 code.
(define (transform-boundary boundary result-node)
  (~a
    (foldl-nodes boundary 
		 (~a "X " main-fun-lbl " \"main\"\n")
		 (lambda (label node res)
		   (string-append
		     res
		     (cond
		       ((literal-node? node) (transform-literal-node boundary node label))
		       ((simple-node? node) (transform-simple-node boundary node label))
		       (else (transform-compound-node boundary node label))))))
    "E " result-node " 1 0 1 " typedval-lbl "\n"))

(define (transform-simple-node boundary node label)
  (let ((opcode (opcode node)))
    (foldl-edges boundary label
		 (~a "N " label " " opcode "\n")
		 (lambda (edge res)
		   (string-append
		     res
		     (format "E ~s ~s ~s ~s ~s\n"
			     label (edge-in-port edge)
			     (edge-out-node edge) (edge-out-port edge)
			     (edge-type-lbl edge)))))))

(define (transform-literal-node boundary node label)
  (let ((value (value node)))
    (foldl-edges boundary label
		 "" (lambda (edge res)
		      (format "L ~s ~s ~s \"~s\"\n"
			      (edge-out-node edge) (edge-out-port edge)
			      (edge-type-lbl edge) value)))))

(define (transform-compound-node boundary label)
  "")
