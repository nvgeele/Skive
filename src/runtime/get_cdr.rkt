#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide get_cdr)

(define get_cdr
  (let*-values
      ([(car-boundary) (make-graph-boundary "")]
       [(car-boundary lbl) (add-node car-boundary (make-simple-node 144))]
       [(car-boundary) (~> car-boundary
                           (add-edge 0 1 lbl 1 conscell-lbl)
                           (add-edge lbl conscell-cdr-idx 0 1 typedval-lbl))]
       [(err-boundary) (make-graph-boundary "")]
       [(err-boundary lbl) (add-node err-boundary (make-literal-node "error"))]
       [(err-boundary) (add-edge err-boundary lbl 1 0 1 typedval-lbl)]
       [(compound-node) (make-tagcase `(,car-boundary ,err-boundary)
                                      #(1 1 1 1 1 0 1))])
    (let*-values ([(boundary) (make-graph-boundary "get_cdr" #f function-lbl)]
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
