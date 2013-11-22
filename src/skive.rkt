#!racket

(require "syntax.rkt"
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
       (let-values ([(gb result-node) (parse-sequence (make-graph-boundary "main" #t)
						      (quote body) (hash))])
	 (let* ((transformed (transform-boundary gb result-node))
		(path (compile-to-dylib
			(string-append (generate-type-definitions-code)
				       stamps (generate-runtime-code)
				       transformed))))
	   (make-thunk path))))]))

;; Currently parses all into one boundary
;; => needs to be program when typing and
;; environments etc are implemented(?)
(define (compile code)
  (let*-values ([(gb result-node)
		 (parse (make-graph-boundary "main" #t) code)]
		[(transformed)
		 (transform-boundary gb result-node)]
		[(path)
		 (compile-to-dylib
		   (string-append (generate-type-definitions-code) stamps
				  (generate-runtime-code) transformed))])
    (make-thunk path)))

(define (parse-and-graphviz code)
  (let*-values ([(dotfile)
		 (path->string (make-temporary-file "skivedot~a" #f "/tmp"))]
		[(output)
		 (open-output-file dotfile #:exists 'truncate)]
		[(pngfile)
		 (string-append dotfile ".png")]
		[(gb result-node)
		 (parse-boundary (make-graph-boundary "main") code (hash))])
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
	      ;(delete-file code-file)(delete-file csrc-file)
	      ;(delete-file cobj-file)(delete-file code-prefix)
	      (if (file-exists? clib-file)
		clib-file
		(error "Could not compile dylib")))
	    (error "Could not compile C source to object file")))
	(error "Could not create C source file.")))))

(define (parse exp)
  (parse-boundary (make-graph-boundary "main")
		  exp (hash)))

(define (parse-boundary gb exp env)
  (cond ((self-evaluating? exp)
	 (parse-self-evaluating gb exp env))
	((let? exp)
	 (parse-let gb exp env))
	((application? exp)
	 (parse-application gb exp env))
	(else (error "Incorrect expression"))))

(define (parse-sequence gb exps env)
  (let loop ((gb gb)
	     (cur (car exps))
	     (rem (cdr exps)))
    (let-values ([(gb result-node) (parse-boundary gb cur env)])
      (if (null? rem)
	(values gb result-node)
	(loop gb (car rem) (cdr rem))))))

(define (parse-let gb exp env)
  (let ((let-defs (let-definitions exp))
	(let-body (let-body exp)))
    (let* ((s (foldl (lambda (def p)
		       (let ((gb (car p))(env (cdr p))
			     (sym (car def))(exp (cadr def)))
			 (let-values ([(gb result-node) (parse-boundary gb exp env)])
			   (cons gb (hash-set env sym result-node)))))
		     (cons gb env)
		     let-defs))
	   (gb (car s))
	   (env (cdr s)))
      (parse-sequence gb let-body env))))

(define (parse-self-evaluating boundary exp env)
  (if (symbol? exp)
    (let ((val-node (hash-ref env exp #f)))
      (if val-node
	(values boundary val-node)
	(error (~a exp " is undefined!"))))
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
      (values gb blbl))))

(define (parse-application boundary exp env)
  (let* ((operator (car exp))
	 (operands (cdr exp))
	 (native (hash-ref natives operator #f)))
    (cond ((not native) (error "Only native functions supported!"))
	  ((not (or (= (length operands) (native-inputs native))
		    (native-reducible? native)))
	   (error "Incorrect amount of arguments for function!"))
	  ((and (> (length operands) (native-inputs native))
		(native-reducible? native))
	   (parse-application boundary (reduce exp) env))
	  (else (let*-values
		  ([(gb inputs) (parse-operands boundary operands env)]
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
(define (parse-operands boundary operands env)
  (if (null? operands)
    (values boundary '())
    (let loop ((boundary boundary)
	       (inputs '())
	       (cur (car operands))
	       (rem (cdr operands)))
      (let-values ([(gb link) (parse-boundary boundary cur env)])
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
