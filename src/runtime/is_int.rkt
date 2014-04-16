#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide is_int)

(define is_int
  (let*-values
      ([(true-boundary) (make-graph-boundary "")]
       [(true-boundary true) (add-node true-boundary (make-literal-node "true"))]
       [(true-boundary) (add-edge true-boundary true 1 0 1 bool-lbl)]
       [(false-boundary) (make-graph-boundary "")]
       [(false-boundary false) (add-node false-boundary (make-literal-node "false"))]
       [(false-boundary) (add-edge false-boundary false 1 0 1 bool-lbl)]
       [(err-boundary) (make-graph-boundary "")]
       [(err-boundary lbl) (add-node err-boundary (make-literal-node "error"))]
       [(err-boundary) (add-edge err-boundary lbl 1 0 1 bool-lbl)]
       [(compound-node) (make-tagcase `(,false-boundary ,true-boundary ,err-boundary)
                                      #(0 1 0 0 0 0 0 0 0))])
    (let*-values ([(boundary) (make-graph-boundary "is_int" #f function-lbl)]
                  [(boundary lbl1) (add-node boundary (make-simple-node 144))]
                  [(boundary lbl2) (add-node boundary (make-simple-node 105))]
                  [(boundary lbl3) (add-node boundary (make-literal-node 0))]
                  [(boundary lbl4) (add-node boundary compound-node)]
                  [(boundary lbl5) (add-node boundary (make-simple-node 143))]
                  [(boundary) (~> boundary
                                  (add-edge 0 1 lbl1 1 frame-lbl)
                                  (add-edge lbl1 2 lbl2 1 typedval-array-lbl)
                                  (add-edge lbl3 1 lbl2 2 int-lbl)
                                  (add-edge lbl2 1 lbl4 1 typedval-lbl)
                                  (add-edge lbl4 1 lbl5 typedval-bool-idx bool-lbl)
                                  (add-edge lbl5 1 0 1 typedval-lbl))])
      boundary)))
