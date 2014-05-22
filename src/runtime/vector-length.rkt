#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide vector-length)

(define error-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary lit) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit 1 0 1 typedval-lbl)))

;; TODO: Check that index is not out of bounds (currently IF1 itself does this)
(define compound-node-vector
  (let*-values
      ([(vector-boundary) (make-graph-boundary "")]
       [(vector-boundary lbl1) (add-node vector-boundary (make-simple-node 144))]
       [(vector-boundary lbl2) (add-node vector-boundary (make-simple-node 143))]
       [(vector-boundary) (~> vector-boundary
                              (add-edge 0 1 lbl1 1 vector-lbl)
                              (add-edge lbl1 vector-size-idx lbl2 typedval-int-idx int-lbl)
                              (add-edge lbl2 1 0 1 typedval-lbl))])
    (make-tagcase `(,vector-boundary ,error-boundary)
                  #(1 1 1 1 1 1 1 1 0))))

(define vector-length
  (let*-values
      ([(boundary) (make-graph-boundary "vector_length" #f function-lbl)]
       [(boundary lbl1) (add-node boundary (make-simple-node 144))]
       [(boundary lbl2) (add-node boundary (make-simple-node 105))]
       [(boundary lbl3) (add-node boundary (make-literal-node 0))]
       [(boundary lbl4) (add-node boundary compound-node-vector)])
    (~> boundary
        (add-edge 0 1 lbl1 1 frame-lbl)
        (add-edge lbl1 2 lbl2 1 typedval-array-lbl)
        (add-edge lbl3 1 lbl2 2 int-lbl)
        (add-edge lbl2 1 lbl4 1 typedval-lbl)
        (add-edge lbl4 1 0 1 typedval-lbl))))
