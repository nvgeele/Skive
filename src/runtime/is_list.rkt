#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide is_list)

(define is_list
  (let*-values
      ([(boundary) (make-graph-boundary "is_list")]
       [(boundary lbl1) (add-node boundary (make-simple-node 144))]
       [(boundary lbl2) (add-node boundary (make-simple-node 105))]
       [(boundary lit1) (add-node boundary (make-literal-node 0))]
       [(boundary lbl3) (add-node boundary (make-simple-node 120))]
       [(boundary lit2) (add-node boundary (make-literal-node "is_list_intern"))])
    (~> boundary
        (add-edge 0 1 lbl1 1 frame-lbl)
        (add-edge lbl1 2 lbl2 1 typedval-array-lbl)
        (add-edge lit1 1 lbl2 2 int-lbl)
        (add-edge lit2 1 lbl3 1 typedval-typedval-function-lbl)
        (add-edge lbl2 1 lbl3 2 typedval-lbl)
        (add-edge lbl3 1 0 1 typedval-lbl))))
