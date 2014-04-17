#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide cons)

(define cons
  (let*-values
      ([(boundary) (make-graph-boundary "cons")]
       [(boundary lbl1) (add-node boundary (make-simple-node 144))]
       [(boundary lbl2) (add-node boundary (make-simple-node 105))]
       [(boundary lit1) (add-node boundary (make-literal-node "1"))]
       [(boundary lbl3) (add-node boundary (make-simple-node 105))]
       [(boundary lit2) (add-node boundary (make-literal-node "0"))]
       [(boundary lbl4) (add-node boundary (make-simple-node 143))]
       [(boundary lbl5) (add-node boundary (make-simple-node 143))])
    (~> boundary
        (add-edge 0 1 lbl1 1 frame-lbl)
        (add-edge lbl1 frame-bind-idx lbl2 1 typedval-array-lbl)
        (add-edge lit1 1 lbl2 2 int-lbl)
        (add-edge lbl1 frame-bind-idx lbl3 1 typedval-array-lbl)
        (add-edge lit2 1 lbl3 2 int-lbl)
        (add-edge lbl3 1 lbl4 1 typedval-lbl) ;; conscell idxes
        (add-edge lbl2 1 lbl4 2 typedval-lbl)
        (add-edge lbl4 1 lbl5 typedval-cons-idx conscell-lbl)
        (add-edge lbl5 1 0 1 typedval-lbl))))
