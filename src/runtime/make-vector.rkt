#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide make-vector)

(define error-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary lit) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit 1 0 1 typedval-lbl)))

(define compound-node-integer
  (let*-values
      ([(integer-boundary) (make-graph-boundary "")]
       [(integer-boundary lit1) (add-node integer-boundary (make-literal-node 1))]
       [(integer-boundary lit2) (add-node integer-boundary (make-literal-node 0))]
       [(integer-boundary lbl1) (add-node integer-boundary (make-simple-node 135))]
       [(integer-boundary lbl2) (add-node integer-boundary (make-simple-node 106))]
       [(integer-boundary lbl3) (add-node integer-boundary (make-simple-node 143))]
       [(integer-boundary lbl4) (add-node integer-boundary (make-simple-node 143))]
       [(integer-boundary) (~> integer-boundary
                               ;;(add-edge 0 1 lbl2 2 int-lbl)
                               (add-edge 0 1 lbl1 1 int-lbl)
                               (add-edge lit1 1 lbl1 2 int-lbl)
                               (add-edge lbl1 1 lbl2 2 int-lbl)
                               (add-edge lit2 1 lbl2 1 int-lbl)
                               (add-edge 0 2 lbl2 3 typedval-lbl)
                               (add-edge lbl2 1 lbl3 vector-content-idx typedval-array-lbl)
                               (add-edge 0 1 lbl3 vector-size-idx int-lbl)
                               (add-edge lbl3 1 lbl4 typedval-vect-idx vector-lbl)
                               (add-edge lbl4 1 0 1 typedval-lbl))])
    (make-tagcase `(,integer-boundary ,error-boundary)
                  #(1 0 1 1 1 1 1 1 1))))

(define make-vector
  (let*-values
      ([(boundary) (make-graph-boundary "make_vector")]
       [(boundary lbl1) (add-node boundary (make-simple-node 144))]
       [(boundary lbl2) (add-node boundary (make-simple-node 105))]
       [(boundary lit1) (add-node boundary (make-literal-node "1"))]
       [(boundary lbl3) (add-node boundary (make-simple-node 105))]
       [(boundary lit2) (add-node boundary (make-literal-node "0"))]
       [(boundary lbl4) (add-node boundary compound-node-integer)])
    (~> boundary
        (add-edge 0 1 lbl1 1 frame-lbl)
        (add-edge lbl1 frame-bind-idx lbl2 1 typedval-array-lbl)
        (add-edge lit1 1 lbl2 2 int-lbl)
        (add-edge lbl1 frame-bind-idx lbl3 1 typedval-array-lbl)
        (add-edge lit2 1 lbl3 2 int-lbl)
        (add-edge lbl3 1 lbl4 1 typedval-lbl)
        (add-edge lbl2 1 lbl4 2 typedval-lbl)
        (add-edge lbl4 1 0 1 typedval-lbl))))
