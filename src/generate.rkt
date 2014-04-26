#!racket

(require "threading.rkt"
	 "syntax.rkt"
	 "node.rkt"
	 "edge.rkt"
	 "compound-node.rkt"
	 "graph-boundary.rkt"
	 "natives.rkt"
	 "typing.rkt"
	 "program.rkt")

(provide generate)

(define (generate exp)
  (let ((program (make-program))
	(boundary (make-graph-boundary "entry")))
    (let-values ([(program gb res) (generate* exp boundary program)])
      (add-boundary program
		    (add-edge gb res 1 0 1 typedval-lbl)))))

(define (generate-sequence seq)
  '())

(define (generate-sequence* seq graph-boundary program)
  (let loop ((cur (car seq))
	     (rem (cdr seq))
	     (gb graph-boundary)
	     (program program))
    (let-values ([(program gb res)
		  (generate* cur gb program)])
      (if (null? rem)
          (values program gb res)
          (loop (car rem) (cdr rem) gb program)))))

;; Always returns the following multiple values:
;; - program
;; - graph boundary
;; - result node
(define (generate* exp graph-boundary program)
  (cond ((lexical-address? exp)
	 (let-values ([(gb res) (generate-lookup graph-boundary
						 (vector-ref exp 0)
						 (vector-ref exp 1))])
	   (values program gb res)))
	((self-evaluating? exp)
	 (let-values ([(gb  res) (generate-self-evaluating graph-boundary exp)])
	   (values program gb res)))
	((lambda? exp)
	 (generate-lambda exp graph-boundary program))
	((if? exp)
	 (generate-if exp graph-boundary program))
	((application? exp)
	 (generate-application exp graph-boundary program))
	(else (error "Incorrect expression -- generate"))))

(define (generate-lookup graph-boundary frame offset)
  (let loop ((frame frame)
	     (gb graph-boundary)
	     (res 0))
    (if (= frame 0)
        (let*-values ([(gb frm-elm) (add-node gb (make-simple-node 144))]
                      [(gb bnd-lit) (add-node gb (make-literal-node offset))]
                      [(gb bnd-elm) (add-node gb (make-simple-node 105))])
          (values (~> gb
                      (add-edge res 1 frm-elm 1 frame-lbl)
                      (add-edge bnd-lit 1 bnd-elm 2 int-lbl)
                      (add-edge frm-elm frame-bind-idx bnd-elm 1 typedval-array-lbl))
                  bnd-elm))
        (let*-values ([(gb-fail) (make-graph-boundary "")]
                      [(gb-fail lit-err) (add-node gb-fail (make-literal-node "error"))]
                      [(gb-fail) (add-edge gb-fail lit-err 1 0 1 frame-lbl)]
                      [(gb-succ) (~> (make-graph-boundary "")
                                     (add-edge 0 1 0 1 frame-lbl))]
                      [(gb frm-elm) (add-node gb (make-simple-node 144))]
                      ;; Assuming back-frame-idx is 1
                      ;; TODO: generate vector automatically or with procedure
                      [(gb tagcase) (add-node gb (make-tagcase `(,gb-fail ,gb-succ ,gb-fail) #(1 0)))])
          (loop (- frame 1)
                (~> gb
                    (add-edge res 1 frm-elm 1 frame-lbl)
                    (add-edge frm-elm frame-prev-idx tagcase 1 back-lbl))
                tagcase)))))

(define (generate-self-evaluating graph-boundary exp)
  (let*-values ([(type-lbl type-idx exp) (cond ((integer? exp) (values int-lbl
                                                                       typedval-int-idx
                                                                       exp))
                                               ((string? exp) (values string-lbl
                                                                      typedval-string-idx
                                                                      exp))
                                               ((null? exp) (values null-lbl
                                                                    typedval-null-idx
                                                                    exp))
                                               ((or (false? exp) (eq? exp #t))
                                                (values bool-lbl typedval-bool-idx (if (false? exp) "F" "T")))
                                               (else (error "Unknown self-evaluating value -- generate")))]
                [(lit-node build-node) (values (make-literal-node exp)
					       (make-simple-node 143))]
		[(gb blbl) (add-node graph-boundary build-node)]
		[(gb llbl) (add-node gb lit-node)]
		[(gb) (add-edge gb llbl 1 blbl type-idx type-lbl)])
    (values gb blbl)))

(define (generate-lambda exp graph-boundary program)
  (let*-values ([(boundary) (make-graph-boundary "temp")]
		[(program gb res) (generate-sequence* (lambda-body exp) boundary program)]
		[(gb) (add-edge gb res 1 0 1 typedval-lbl)]
		[(label) (next-label program)]
		[(str-label) (next-label-str program)]
		[(program) (add-boundary program (set-boundary-name gb str-label))]
		[(gb lit1) (add-node graph-boundary (make-literal-node label))]
		[(gb lit2) (add-node gb (make-literal-node (length (lambda-args exp))))]
		[(gb bld1) (add-node gb (make-simple-node 143))]
		[(gb bld2) (add-node gb (make-simple-node 143))])
    (values program
	    (~> gb
		(add-edge lit1 1 bld1 closure-func-idx int-lbl)
		(add-edge lit2 1 bld1 closure-args-idx int-lbl)
		(add-edge lit2 1 bld1 closure-framesize-idx int-lbl)
		(add-edge 0    1 bld1 closure-env-idx frame-lbl)
		(add-edge bld1 1 bld2 typedval-func-idx closure-lbl))
	    bld2)))

(define (generate-args arg-exps graph-boundary program)
  (if (null? arg-exps)
      (values program graph-boundary '())
      (let loop ((cur (car arg-exps))
                 (rem (cdr arg-exps))
                 (lst '())
                 (gb graph-boundary)
                 (program program))
        (let-values ([(program gb res) (generate* cur gb program)])
          (if (null? rem)
              (values program gb (reverse (cons res lst)))
              (loop (car rem) (cdr rem) (cons res lst) gb program))))))

(define (generate-quoted-list graph-boundary list)
  (let ((list (reverse list)))
    (let-values ([(gb nlbl) (generate-self-evaluating graph-boundary null)])
      (let loop ((cur (car list))
                 (rem (cdr list))
                 (gb gb)
                 (prev nlbl))
        (let*-values ([(gb res) (generate-self-evaluating gb cur)]
                      [(gb cons-bld) (add-node gb (make-simple-node 143))]
                      [(gb tval-bld) (add-node gb (make-simple-node 143))]
                      [(gb) (~> gb
                                (add-edge prev 1 cons-bld 2 typedval-lbl)
                                (add-edge res 1 cons-bld 1 typedval-lbl)
                                (add-edge cons-bld 1 tval-bld typedval-cons-idx conscell-lbl))])
          (if (null? rem)
              (values gb tval-bld)
              (loop (car rem) (cdr rem) gb tval-bld)))))))

(define (generate-list program graph-boundary list)
  (let ((list (reverse list)))
    (let-values ([(gb nlbl) (generate-self-evaluating graph-boundary null)])
      (let loop ((cur (car list))
                 (rem (cdr list))
                 (gb gb)
                 (program program)
                 (prev nlbl))
        (let*-values ([(program gb res) (generate* cur gb program)]
                      [(gb cons-bld) (add-node gb (make-simple-node 143))]
                      [(gb tval-bld) (add-node gb (make-simple-node 143))]
                      [(gb) (~> gb
                                (add-edge prev 1 cons-bld 2 typedval-lbl)
                                (add-edge res 1 cons-bld 1 typedval-lbl)
                                (add-edge cons-bld 1 tval-bld typedval-cons-idx conscell-lbl))])
          (if (null? rem)
              (values program gb tval-bld)
              (loop (car rem) (cdr rem) gb program tval-bld)))))))

(define (generate-vector program graph-boundary vector)
  (let ((vector (reverse vector))
        (size (length vector)))
    (let loop ((cur (car vector))
               (rem (cdr vector))
               (gb graph-boundary)
               (program program)
               (labels '()))
      (let-values ([(program gb lbl) (generate* cur gb program)])
        (if (null? rem)
            (let*-values
                ([(gb abuild) (add-node gb (make-simple-node 103))] ;; ABuild
                 [(gb lit1) (add-node gb (make-literal-node 0))]
                 [(gb vbuild) (add-node gb (make-simple-node 143))] ;; RBuild
                 [(gb lit2) (add-node gb (make-literal-node size))]
                 [(gb tvbuild) (add-node gb (make-simple-node 143))] ;; RBuild
                 [(gb) (~> (foldl (lambda (label port gb)
                                    (add-edge gb label 1 abuild port typedval-lbl))
                                  gb
                                  (cons lbl labels)
                                  (range 2 (+ 2 size)))
                           (add-edge lit1 1 abuild 1 int-lbl)
                           (add-edge lit2 1 vbuild vector-size-idx int-lbl)
                           (add-edge abuild 1 vbuild vector-content-idx typedval-array-lbl)
                           (add-edge vbuild 1 tvbuild typedval-vect-idx vector-lbl))])
              (values program gb tvbuild))
            (loop (car rem) (cdr rem) gb program (cons lbl labels)))))))

;; TODO: check if correct amount of arguments are given (by transforming to apply?)
(define (generate-application exp graph-boundary program)
  (cond
   ((eq? (appl-op exp) 'vector)
    (let-values ([(program gb lbl)
                  (generate-vector program graph-boundary (cdr exp))])
      (values program gb lbl)))
   ((eq? (appl-op exp) 'list)
    (let-values ([(program gb lbl)
                  (generate-list program graph-boundary (cdr exp))])
      (values program gb lbl)))
   ((eq? (appl-op exp) 'quote)
    (cond ((symbol? (quote-value exp))
           (error "We don't have symbols yet -- generate"))
          ((null? (quote-value exp))
           (let-values ([(gb lbl)
                         (generate-self-evaluating graph-boundary null)])
             (values program gb lbl)))
          (else
           (let-values ([(gb lbl)
                         (generate-quoted-list graph-boundary (cadr exp))])
             (values program gb lbl)))))
   (else
    (let*-values ([(gb-call) (make-graph-boundary "")]
                  [(gb-fail) (make-graph-boundary "")]
                  [(num-args) (length (appl-args exp))]
                  [(program gb op-res)
                   (generate* (appl-op exp) graph-boundary program)]
                  [(program gb args)
                   (generate-args (appl-args exp) gb program)]
                  [(gb-call cls-elm) (add-node gb-call (make-simple-node 144))]
                  [(gb-call bnd-lit) (add-node gb-call (make-literal-node "0"))]
                  [(gb-call bnd-bld) (add-node gb-call (make-simple-node 103))]
                  [(gb-call bck-bld) (add-node gb-call (make-simple-node 143))]
                  [(gb-call frm-bld) (add-node gb-call (make-simple-node 143))]
                  [(gb-call cll-lit) (add-node gb-call (make-literal-node "call"))]
                  [(gb-call call)    (add-node gb-call (make-simple-node 120))]
                  [(gb-call) (~> gb-call
                                 (add-edge 0 1 cls-elm 1 closure-lbl)
                                 (add-edge bnd-lit 1 bnd-bld 1 int-lbl)
                                 (add-edge cls-elm closure-env-idx bck-bld back-frame-idx frame-lbl)
                                 (add-edge bnd-bld 1 frm-bld frame-bind-idx typedval-array-lbl)
                                 (add-edge bck-bld 1 frm-bld frame-prev-idx back-lbl)
                                 (add-edge cls-elm closure-func-idx call 2 int-lbl)
                                 (add-edge frm-bld 1 call 3 frame-lbl)
                                 (add-edge cll-lit 1 call 1 call-function-lbl)
                                 (add-edge call 1 0 1 typedval-lbl))]
                  [(gb-call) (foldl (lambda (p gb)
                                      (add-edge gb 0 p bnd-bld p typedval-lbl))
                                    gb-call
                                    (range 2 (+ 2 num-args)))]
                  [(gb-fail err-lit) (add-node gb-fail (make-literal-node "error"))]
                  [(gb-fail) (add-edge gb-fail err-lit 1 0 1 typedval-lbl)]
                  ;; TODO: make sure vector gets generated automatically!
                  [(cn) (make-tagcase `(,gb-fail ,gb-call ,gb-fail) #(0 0 0 0 0 0 1))]
                  [(gb cn) (add-node gb cn)]
                  [(gb) (car (foldl (lambda (node c)
                                      (cons (add-edge (car c) node 1 cn (cdr c) typedval-lbl)
                                            (+ 1 (cdr c))))
                                    (cons gb 2) args))]
                  [(gb) (~> gb
                            (add-edge op-res 1 cn 1 typedval-lbl))])
      (values program gb cn)))))

(define (generate-if exp graph-boundary program)
  (let*-values ([(program gb test-res)
		 (generate* (if-condition exp) graph-boundary program)]
		[(gb litt-node) (add-node gb (make-literal-node "is_false_nat"))]
		[(gb call-node) (add-node gb (make-simple-node 120))]
		[(gb int-node)  (add-node gb (make-simple-node 129))]
		[(gb-select) (~> (make-graph-boundary "")
				 (add-edge 0 2 0 1 int-lbl))]
		[(gb-consequent) (make-graph-boundary "")]
		[(program gb-consequent c-res)
		 (generate* (if-consequent exp) (make-graph-boundary "") program)]
		[(gb-consequent)
		 (add-edge gb-consequent c-res 1 0 1 typedval-lbl)]
		[(program gb-alternative a-res)
		 (generate* (if-alternative exp) (make-graph-boundary "") program)]
		[(gb-alternative)
		 (add-edge gb-alternative a-res 1 0 1 typedval-lbl)]
		[(gb sel-node)
		 (add-node gb (make-select `(,gb-select
                                             ,gb-consequent
                                             ,gb-alternative)
					   #(0 1 2)))]
		[(gb)
		 (~> gb
		     (add-edge litt-node 1 call-node 1 is-false-nat-func-lbl)
		     (add-edge test-res 1 call-node 2 typedval-lbl)
		     (add-edge call-node 1 int-node 1 bool-lbl)
		     (add-edge int-node 1 sel-node 2 int-lbl)
		     (add-edge 0 1 sel-node 1 frame-lbl))])
    (values program gb sel-node)))
