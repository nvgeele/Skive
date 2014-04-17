#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide divide)

;; divide := 122
(define op 122)

(define error-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary lit) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit 1 0 1 typedval-lbl)))

(define compound-node-int
  (let*-values
      ([(int-boundary) (make-graph-boundary "")]
       [(int-boundary lbl1) (add-node int-boundary (make-simple-node op))]
       [(int-boundary lbl2) (add-node int-boundary (make-simple-node 143))]
       [(int-boundary) (~> int-boundary
                           (add-edge 0 2 lbl1 1 int-lbl)
                           (add-edge 0 1 lbl1 2 int-lbl)
                           (add-edge lbl1 1 lbl2 typedval-int-idx int-lbl)
                           (add-edge lbl2 1 0 1 typedval-lbl))]
       [(float-boundary) (make-graph-boundary "")]
       [(float-boundary lbl1) (add-node float-boundary (make-simple-node 123))]
       [(float-boundary lbl2) (add-node float-boundary (make-simple-node op))]
       [(float-boundary lbl3) (add-node float-boundary (make-simple-node 143))]
       [(float-boundary) (~> float-boundary
                             (add-edge 0 2 lbl1 1 int-lbl)
                             (add-edge lbl1 1 lbl2 1 float-lbl)
                             (add-edge 0 1 lbl2 2 float-lbl)
                             (add-edge lbl2 1 lbl3 typedval-float-idx float-lbl)
                             (add-edge lbl3 1 0 1 typedval-lbl))])
    (make-tagcase `(,int-boundary ,float-boundary, error-boundary)
                  #(2 0 1 2 2 2 2 2 2))))

(define compound-node-float
  (let*-values
      ([(int-boundary) (make-graph-boundary "")]
       [(int-boundary lbl1) (add-node int-boundary (make-simple-node 123))]
       [(int-boundary lbl2) (add-node int-boundary (make-simple-node op))]
       [(int-boundary lbl3) (add-node int-boundary (make-simple-node 143))]
       [(int-boundary) (~> int-boundary
                           (add-edge 0 1 lbl1 1 int-lbl)
                           (add-edge 0 2 lbl2 1 float-lbl)
                           (add-edge lbl1 1 lbl2 2 float-lbl)
                           (add-edge lbl2 1 lbl3 typedval-float-idx float-lbl)
                           (add-edge lbl3 1 0 1 typedval-lbl))]
       [(float-boundary) (make-graph-boundary "")]
       [(float-boundary lbl1) (add-node float-boundary (make-simple-node op))]
       [(float-boundary lbl2) (add-node float-boundary (make-simple-node 143))]
       [(float-boundary) (~> float-boundary
                             (add-edge 0 2 lbl1 1 float-lbl)
                             (add-edge 0 1 lbl1 2 float-lbl)
                             (add-edge lbl1 1 lbl2 typedval-float-idx float-lbl)
                             (add-edge lbl2 1 0 1 typedval-lbl))])
    (make-tagcase `(,int-boundary ,float-boundary ,error-boundary)
                  #(2 0 1 2 2 2 2 2 2))))

(define compound-node
  (let*-values
      ([(int-boundary) (make-graph-boundary "")]
       [(int-boundary lbl1) (add-node int-boundary compound-node-int)]
       [(int-boundary) (~> int-boundary
                           (add-edge 0 2 lbl1 1 typedval-lbl)
                           (add-edge 0 1 lbl1 2 int-lbl)
                           (add-edge lbl1 1 0 1 typedval-lbl))]
       [(float-boundary) (make-graph-boundary "")]
       [(float-boundary lbl1) (add-node float-boundary compound-node-float)]
       [(float-boundary) (~> float-boundary
                             (add-edge 0 2 lbl1 1 typedval-lbl)
                             (add-edge 0 1 lbl1 2 float-lbl)
                             (add-edge lbl1 1 0 1 typedval-lbl))])
    (make-tagcase `(,int-boundary ,float-boundary ,error-boundary)
                  #(2 0 1 2 2 2 2 2 2))))

(define divide
  (let*-values
      ([(boundary) (make-graph-boundary "divide")]
       [(boundary lbl1) (add-node boundary (make-simple-node 144))]
       [(boundary lbl2) (add-node boundary (make-simple-node 105))]
       [(boundary lit1) (add-node boundary (make-literal-node "1"))]
       [(boundary lbl3) (add-node boundary (make-simple-node 105))]
       [(boundary lit2) (add-node boundary (make-literal-node "0"))]
       [(boundary lbl4) (add-node boundary compound-node)])
    (~> boundary
        (add-edge 0 1 lbl1 1 frame-lbl)
        (add-edge lbl1 frame-bind-idx lbl2 1 typedval-array-lbl)
        (add-edge lit1 1 lbl2 2 int-lbl)
        (add-edge lbl1 frame-bind-idx lbl3 1 typedval-array-lbl)
        (add-edge lit2 1 lbl3 2 int-lbl)
        (add-edge lbl3 1 lbl4 1 typedval-lbl)
        (add-edge lbl2 1 lbl4 2 typedval-lbl)
        (add-edge lbl4 1 0 1 typedval-lbl))))
