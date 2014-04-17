#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide is_false)

(define is_false
  (let*-values
      ([(true-boundary) (make-graph-boundary "")]
       [(true-boundary build) (add-node true-boundary (make-simple-node 143))]
       [(true-boundary false) (add-node true-boundary (make-literal-node "false"))]
       [(true-boundary) (~> true-boundary
                            (add-edge false 1 build typedval-bool-idx bool-lbl)
                            (add-edge build 1 0 1 typedval-lbl))]
       [(false-boundary) (make-graph-boundary "")]
       [(false-boundary lbl1) (add-node false-boundary (make-simple-node 139))]
       [(false-boundary lbl2) (add-node false-boundary (make-simple-node 143))]
       [(false-boundary) (~> false-boundary
                             (add-edge 0 1 lbl1 1 bool-lbl)
                             (add-edge lbl1 1 lbl2 typedval-bool-idx bool-lbl)
                             (add-edge lbl2 1 0 1 typedval-lbl))]
       [(compound-node) (make-tagcase `(,false-boundary ,true-boundary)
                                      #(1 1 1 1 0 1 1 1 1))])
    (let*-values ([(boundary) (make-graph-boundary "is_false" #f function-lbl)]
                  [(boundary lbl1) (add-node boundary (make-simple-node 144))]
                  [(boundary lbl2) (add-node boundary (make-simple-node 105))]
                  [(boundary lbl3) (add-node boundary (make-literal-node 0))]
                  [(boundary lbl4) (add-node boundary compound-node)]
                  [(boundary) (~> boundary
                                  (add-edge 0 1 lbl1 1 frame-lbl)
                                  (add-edge lbl1 2 lbl2 1 typedval-array-lbl)
                                  (add-edge lbl3 1 lbl2 2 int-lbl)
                                  (add-edge lbl2 1 lbl4 1 typedval-lbl)
                                  (add-edge lbl4 1 0 1 typedval-lbl))])
      boundary)))
