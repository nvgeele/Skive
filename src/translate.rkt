#!racket

(provide translate)

(require "program.rkt"
	 "node.rkt"
	 "edge.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "typing.rkt"
	 "runtime.rkt")

(define stamps
  "C$ D Nodes are DFOrdered\n")

(define (translate program)
  (let ((boundaries (program-boundaries program)))
    (string-append
      (generate-type-definitions-code) "\n"
      stamps "\n"
      (generate-runtime-code) "\n"
      (foldl (lambda (boundary code)
	       (string-append code
			      (translate-boundary boundary)
			      "\n"))
	     ""
	     boundaries)
      (make-call-function program)
      (make-main-function program))))

(define (translate-boundary boundary)
  (let ((sorted (filter (lambda (i) (not (= i 0)))
			(range 1 (label-counter boundary)))))
			;(topological-sort boundary))))
    (~a "G " function-lbl "\t\"" (graph-boundary-name boundary) "\"\n"
	(foldl (lambda (lbl str)
		 (~a str
		     (let ((node (graph-node boundary lbl)))
		       (cond ((literal-node? node)
			      "")
			     ((simple-node? node)
			      (translate-simple-node boundary node lbl))
			     (else
			       (translate-compound-node boundary node lbl))))))
	       ""
	       sorted)
	(foldl-edges-to boundary 0
			"" (lambda (edge res)
			     (~a res
				 "E\t" (edge-in-node edge) " " (edge-in-port edge)
				 "\t" (edge-out-node edge) " " (edge-out-port edge)
				 "\t" (edge-type-lbl edge) "\n"))))))

(define (translate-boundary* boundary)
  (~a
    (foldl-nodes boundary
		 (~a "G " function-lbl " \"" (graph-boundary-name boundary) "\"\n")
		 (lambda (label node res)
		   (string-append
		     res
		     (cond ((literal-node? node)
			    (translate-literal-node boundary node label))
			   ((simple-node? node)
			    (translate-simple-node boundary node label))
			   (else
			     (translate-compound-node boundary node label))))))
    (foldl-edges boundary 0 ""
		 (lambda (edge res)
		   (format "E\t~s ~s\t~s ~s\t~s\n"
			   (edge-in-node edge) (edge-in-port edge)
			   (edge-out-node edge) (edge-out-port edge)
			   (edge-type-lbl edge))
		   ))))

(define (translate-literal-node boundary node label)
  (let ((value (value node)))
    (foldl-edges boundary label
		 "" (lambda (edge res)
		      (~a "L\t\t" (edge-out-node edge) " " (edge-out-port edge)
			  "\t" (edge-type-lbl edge) " \"" value "\"\n")))))

(define (translate-simple-node boundary node label)
  (let ((opcode (opcode node)))
    (foldl-edges-to boundary label
		    (~a "N " label "\t" opcode "\n")
		    (lambda (edge res)
		      (let ((in-node (graph-node boundary (edge-in-node edge))))
			(~a res
			    (if (literal-node? in-node)
			      (~a "L\t\t" (edge-out-node edge) " " (edge-out-port edge)
				  "\t" (edge-type-lbl edge) " \"" (value in-node) "\"\n")
			      (~a "E\t" (edge-in-node edge) " " (edge-in-port edge)
				  "\t" (edge-out-node edge) " " (edge-out-port edge)
				  "\t" (edge-type-lbl edge) "\n"))))))))

(define (translate-simple-node* boundary node label)
  (let ((opcode (opcode node)))
    (foldl-edges boundary label
		 (~a "N " label "\t" opcode "\n")
		 (lambda (edge res)
		   (string-append
		     res
		     (format "E\t~s ~s\t~s ~s\t~s\n"
			     (edge-in-node edge) (edge-in-port edge)
			     (edge-out-node edge) (edge-out-port edge)
			     (edge-type-lbl edge)))))))

(define (translate-compound-node boundary node label)
  "")

(define (make-call-function program)
  (~a "G " call-function-lbl "\t\"call\"\n"
      "N 1\t143\n"
      "L\t\t1 2\t4 \"1\"\n"
      "E\t1 1\t0 1\t" typedval-lbl "\n")
  (~a "G " call-function-lbl "\t\"call\"\n"
      "N 1\t120\n"
      "E\t0 2\t1 2\t" frame-lbl "\n"
      "L\t\t1 1\t" function-lbl " \"plus\"\n"
      "E\t1 1\t0 1\t" typedval-lbl "\n"))

#|(define (make-call-function program)
  (let ((funcs (append (map (lambda (n)
			      (native-name (cdr n)))
			    natives-list)
		       (build-list (- (program-count program)
				      (length natives-list))
				   (lambda (i)
				     (~a "proc" (+ i (length natives-list) -1)))))))
    (~a "G " call-function-lbl "\t\"call\"\n"
	(let loop ((cur (car funcs))
		   (rem (cdr funcs)))
	  (~a "{\n"
	      "G 0\n"
	      "E\t0 1\t0 1\t" int-lbl "\n"
	      "G 0\n"
	      "N 1\t120\n"
	      "L\t\t1 1\t" function-lbl " \"" cur "\"\n"
	      "E\t0 2\t1 2\t" frame-lbl "\n"
	      "E\t1 1\t0 1\t" typedval-lbl "\n"
	      "G 0\n"
	      (if (null? rem)
		(~a "L\t\t0 1\t" typedval-lbl " \"error\"\n")
		(~a "N 1\t135\n"
		    "E\t0 1\t1 1\t" int-lbl "\n"
		    "L\t\t1 2\t" int-lbl " \"1\"\n"
		    (loop (car rem) (cdr rem))
		    #|"E\t1 1\t2 1\t" int-lbl "\n"
		    "E\t0 2\t2 2\t" frame-lbl "\n"
		    "E\t2 1\t0 1\t" typedval-lbl "\n"|#
		    ))
	      "} 2 1 3 0 1 2\n"
	      "E\t0 1\t2 1\t" int-lbl "\n"
	      "E\t0 2\t2 2\t" frame-lbl "\n"
	      "E\t2 1\t0 1\t" typedval-lbl "\n")))))|#

(define (make-call-function* program)
  (~a "G " call-function-lbl "\t\"call\"\n"
      "{ Compound 1 1\n"
      "G 0\n"
      "E\t0 1\t0 1\t4\n"
      (foldl (lambda (label str)
	       (~a str
		   "G 0\n"
		   "N 1\t120\n"
		   "L\t\t1 1\t" function-lbl " \"" label "\"\n"
		   "E\t0 2\t1 2\t" frame-lbl "\n"
		   "E\t1 1\t0 1\t" typedval-lbl "\n"))
	     ""
	     (append (map (lambda (n)
			    (native-name (cdr n)))
			  natives-list)
		     (build-list (- (program-count program)
				    (length natives-list))
				 (lambda (i)
				   (~a "proc" (+ i (length natives-list) -1))))))
      "} 1 1 "
      (program-count program)
      (foldl (lambda (i str)
	       (~a str " " i))
	     ""
	     (range 0 (+ (program-count program) 1)))
      "\n"
      "E\t0 1\t1 1\t" int-lbl "\n"
      "E\t0 2\t1 2\t" frame-lbl "\n"
      "E\t1 1\t0 1\t" typedval-lbl "\n\n"))

;; The main function build ups the initial environment
;; and calls the main function with it.
(define (make-main-function* program)
  (~a "X " main-function-lbl "\t\"main\"\n"
      ;; First we build an empty frame
      "N 1\t143\n"
      "L\t\t1 " back-null-idx "\t" null-lbl " \"nil\"\n"
      "N 2\t103\n"
      "L\t\t2 1\t" int-lbl " \"0\"\n"
      "N 3\t143\n"
      "E\t1 1\t3 " frame-prev-idx "\t" back-lbl "\n"
      "E\t2 1\t3 " frame-bind-idx "\t" typedval-array-lbl "\n"
      ;; Empty frame is now in node 3
      ;; cnt := next free id
      ;; ids := list of all typedval ids
      (let-values ([(str cnt ids) (make-env-nodes)])
	(~a str
	    "N " cnt "\t103\n"
	    "L\t\t" cnt " 1\t" int-lbl " \"" (length natives-list) "\"\n"
	    (car
	      (foldl (lambda (id cell)
		       (cons
			 (~a 
			   (car cell)
			   "E\t" id " 1\t" cnt " " (cdr cell) "\t" typedval-lbl "\n")
			 (+ 1 (cdr cell))))
		     (cons "" 2)
		     ids))
	    "N " (+ cnt 1) "\t143\n"
	    "E\t1 1\t" (+ cnt 1) " " frame-prev-idx "\t" back-lbl "\n"
	    "E\t" cnt " 1\t" (+ cnt 1) " " frame-bind-idx "\t" frame-lbl "\n"
	    "N" (+ cnt 2) "\t120\n"
	    "L\t\t" (+ cnt 2) " 1\t" function-lbl " \"entry\"\n"
	    "E\t" (+ cnt 1) " 1\t" (+ cnt 2) " 2\t" typedval-array-lbl "\n"
	    "E\t" (+ cnt 2) " 1\t0 1\t" typedval-lbl "\n"))))

(define (make-main-function program)
  (~a "X " main-function-lbl "\t\"main\"\n"
      ;; First, we build a single frame, with no backlink
      ;; and only a single value in the frame, null.
      ;; This frame will be used as the bound environment
      ;; for the closures of native functions.
      "N 1\t143\n"
      "L\t\t1 " back-null-idx "\t" null-lbl " \"nil\"\n"
      "N 2\t143\n"
      "L\t\t2 " typedval-null-idx "\t" null-lbl " \"nil\"\n"
      "N 3\t103\n"
      "L\t\t3 1\t" int-lbl " \"1\"\n"
      "E\t2 1\t3 2\t" typedval-lbl "\n"
      "N 4\t143\n"
      "E\t1 1\t4 " frame-prev-idx "\t" back-lbl "\n"
      "E\t3 1\t4 " frame-bind-idx "\t" typedval-array-lbl "\n"
      ;; The single frame now resides in node 4.
      ;; cnt := next free id
      ;; ids := list of all typedval ids
      (let-values ([(str cnt ids) (make-env-nodes)])
	(~a str
	    "N " cnt "\t103\n"
	    "L\t\t" cnt " 1\t" int-lbl " \"" (length natives-list) "\"\n"
	    (car
	      (foldl (lambda (id cell)
		       (cons
			 (~a 
			   (car cell)
			   "E\t" id " 1\t" cnt " " (cdr cell) "\t" typedval-lbl "\n")
			 (+ 1 (cdr cell))))
		     (cons "" 2)
		     ids))
	    "N " (+ cnt 1) "\t143\n"
	    "E\t1 1\t" (+ cnt 1) " " frame-prev-idx "\t" back-lbl "\n"
	    "E\t" cnt " 1\t" (+ cnt 1) " " frame-bind-idx "\t" typedval-array-lbl "\n"
	    "N " (+ cnt 2) "\t120\n"
	    "L\t\t" (+ cnt 2) " 1\t" function-lbl " \"entry\"\n"
	    "E\t" (+ cnt 1) " 1\t" (+ cnt 2) " 2\t" frame-lbl "\n"
	    "E\t" (+ cnt 2) " 1\t0 1\t" typedval-lbl "\n"))))

(define (make-env-nodes)
  (let loop ((cur (car natives-list))
	     (rem (cdr natives-list))
	     (str "")(cnt 5)(ids '()) ;; CHANGE BACK TO 4 pls thx bai
	     (id 0))
    (let ((str
	    (~a str
		"N " cnt "\t143\n"
		"E\t4 1\t" cnt " " closure-env-idx "\t" frame-lbl "\n" ;; CHANGE BACK TO 3 pls thx bai
		"L\t\t" cnt " " closure-func-idx "\t" int-lbl " \"" id "\"\n"
		"L\t\t" cnt " " closure-args-idx "\t" int-lbl " \"" (native-inputs (cdr cur)) "\"\n"
		"L\t\t" cnt " " closure-framesize-idx "\t" int-lbl " \"" (native-inputs (cdr cur)) "\"\n"
		"N " (+ cnt 1) "\t143\n"
		"E\t" cnt " 1\t" (+ cnt 1) " " typedval-func-idx "\t" closure-lbl "\n")))
    (if (null? rem)
      (values str (+ cnt 2) (cons (+ cnt 1) ids))
      (loop (car rem) (cdr rem)
	    str
	    (+ cnt 2)
	    (cons (+ cnt 1) ids)
	    (+ id 1))))))
