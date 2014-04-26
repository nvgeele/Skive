#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide apply)

;; How apply works:
;; Apply receives two typedval's as argument, one must be a closure,
;; the other must be a vector. If both types are correct, apply will
;; check that the vector's length is equal to the amount of
;; arguments needed by the closure. If they are equal, the procedure
;; integer will be extracted from the closure, the vector will be
;; turned into a frame, and the call procedure will be called. If
;; they are not equal, or an argument has an incorrect type, an error
;; value will be generated.
;; No resizeing of the vector's array is needed.

(define error-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary lit) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit 1 0 1 typedval-lbl)))

;; 0 - 2 : function integer
;; 0 - 3 : procedure frame
;; 0 - 4 : vector from typedval
(define compound-node-call
  (let*-values
      ([(sb) (make-graph-boundary "")]
       [(sb) (add-edge sb 0 1 0 1 int-lbl)]
       [(cb) (make-graph-boundary "")] ;; cb =  call-boundary
       [(cb back-build) (add-node cb (make-simple-node 143))]
       [(cb frame-build) (add-node cb (make-simple-node 143))]
       [(cb call-node) (add-node cb (make-simple-node 120))]
       [(cb lit-node) (add-node cb (make-literal-node "call"))]
       [(cb) (~> cb
                 (add-edge 0 3 back-build back-frame-idx frame-lbl)
                 (add-edge back-build 1 frame-build frame-prev-idx back-lbl)
                 (add-edge 0 4 frame-build frame-bind-idx typedval-array-lbl)
                 (add-edge frame-build 1 call-node 3 frame-lbl)
                 (add-edge 0 2 call-node 2 int-lbl)
                 (add-edge lit-node 1 call-node 1 call-function-lbl)
                 (add-edge call-node 1 0 1 typedval-lbl))])
    (make-select `(,sb ,error-boundary ,cb)
                 #(0 2 1))))

;; 0 - 1 : The vector typedval
;; 0 - 2 : The procedure typedval
(define compound-node-vector
  (let*-values
      ([(vb) (make-graph-boundary "")] ;; vb = vector-boundary
       [(vb vect-elements) (add-node vb (make-simple-node 144))]
       [(vb closure-elements) (add-node vb (make-simple-node 144))]
       [(vb equal) (add-node vb (make-simple-node 124))]
       [(vb int) (add-node vb (make-simple-node 129))]
       [(vb call) (add-node vb compound-node-call)]
       [(vb) (~> vb
                 (add-edge 0 1 vect-elements 1 vector-lbl)
                 (add-edge 0 2 closure-elements 1 closure-lbl)
                 (add-edge vect-elements vector-size-idx equal 1 int-lbl)
                 (add-edge closure-elements closure-args-idx equal 2 int-lbl)
                 (add-edge equal 1 int 1 int-lbl)
                 (add-edge int 1 call 1 int-lbl)
                 (add-edge closure-elements closure-func-idx call 2 int-lbl)
                 (add-edge closure-elements closure-env-idx call 3 frame-lbl)
                 (add-edge vect-elements vector-content-idx call 4 typedval-array-lbl)
                 (add-edge call 1 0 1 typedval-lbl))])
    (make-tagcase `(,vb ,error-boundary)
                  #(1 1 1 1 1 1 1 1 0))))

(define compound-node-closure
  (let*-values
      ([(closure-boundary) (make-graph-boundary "")]
       [(closure-boundary lbl1) (add-node closure-boundary compound-node-vector)]
       [(closure-boundary) (~> closure-boundary
                               (add-edge 0 1 lbl1 2 closure-lbl)
                               (add-edge 0 2 lbl1 1 typedval-lbl)
                               (add-edge lbl1 1 0 1 typedval-lbl))])
    (make-tagcase `(,closure-boundary ,error-boundary)
                  #(1 1 1 1 1 1 0 1 1))))

(define apply
  (let*-values
      ([(boundary) (make-graph-boundary "apply")]
       [(boundary lbl1) (add-node boundary (make-simple-node 144))]
       [(boundary lbl2) (add-node boundary (make-simple-node 105))]
       [(boundary lit1) (add-node boundary (make-literal-node "1"))]
       [(boundary lbl3) (add-node boundary (make-simple-node 105))]
       [(boundary lit2) (add-node boundary (make-literal-node "0"))]
       [(boundary lbl4) (add-node boundary compound-node-closure)])
    (~> boundary
        (add-edge 0 1 lbl1 1 frame-lbl)
        (add-edge lbl1 frame-bind-idx lbl2 1 typedval-array-lbl)
        (add-edge lit1 1 lbl2 2 int-lbl)
        (add-edge lbl1 frame-bind-idx lbl3 1 typedval-array-lbl)
        (add-edge lit2 1 lbl3 2 int-lbl)
        (add-edge lbl3 1 lbl4 1 typedval-lbl)
        (add-edge lbl2 1 lbl4 2 typedval-lbl)
        (add-edge lbl4 1 0 1 typedval-lbl))))
