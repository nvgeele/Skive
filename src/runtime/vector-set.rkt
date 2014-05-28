#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide vector-set)

(define error
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary lit1) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit1 1 0 1 typedval-lbl)))

(define vect-compound-node
  (let*-values
      ([(boundary)
        (make-graph-boundary "")]
       [(boundary vector-elements)
        (add-node boundary (make-simple-node 144))]
       [(boundary array-replace)
        (add-node boundary (make-simple-node 113))]
       [(boundary vector-build)
        (add-node boundary (make-simple-node 143))]
       [(boundary typedval-build)
        (add-node boundary (make-simple-node 143))]
       [(boundary)
        (~> boundary
            (add-edge 0 1
                      vector-elements 1 vector-lbl)
            (add-edge vector-elements vector-content-idx
                      array-replace 1 typedval-array-lbl)
            (add-edge 0 2
                      array-replace 2 int-lbl)
            (add-edge 0 3
                      array-replace 3 typedval-lbl)

            (add-edge array-replace 1
                      vector-build vector-content-idx typedval-array-lbl)
            (add-edge vector-elements vector-size-idx
                      vector-build vector-size-idx int-lbl)

            (add-edge vector-build 1
                      typedval-build typedval-vect-idx vector-lbl)

            (add-edge typedval-build 1
                      0 1 typedval-lbl))])
    (make-tagcase `(,boundary ,error)
                  #(1 1 1 1 1 1 1 1 0))))

(define int-compound-node
  (let*-values
      ([(boundary)
        (make-graph-boundary "")]
       [(boundary compound-node)
        (add-node boundary vect-compound-node)]
       [(boundary)
        (~> boundary
            (add-edge 0 1
                      compound-node 2 int-lbl)
            (add-edge 0 2
                      compound-node 1 typedval-lbl)
            (add-edge 0 3
                      compound-node 3 typedval-lbl)
            (add-edge compound-node 1
                      0 1 typedval-lbl))])
    (make-tagcase `(,boundary ,error)
                  #(1 0 1 1 1 1 1 1 1))))

(define vector-set
  (let*-values
      ([(boundary)
        (make-graph-boundary "vector_set")]
       [(boundary frame-elements)
        (add-node boundary (make-simple-node 144))]
       [(boundary array-elements3)
        (add-node boundary (make-simple-node 105))]
       [(boundary literal-node3)
        (add-node boundary (make-literal-node "2"))]
       [(boundary array-elements2)
        (add-node boundary (make-simple-node 105))]
       [(boundary literal-node2)
        (add-node boundary (make-literal-node "1"))]
       [(boundary array-elements1)
        (add-node boundary (make-simple-node 105))]
       [(boundary literal-node1)
        (add-node boundary (make-literal-node "0"))]

       [(boundary compound-node)
        (add-node boundary int-compound-node)]
       )
    (~> boundary
        (add-edge 0 1 frame-elements 1 frame-lbl)

        (add-edge frame-elements frame-bind-idx
                  array-elements3 1 typedval-array-lbl)
        (add-edge literal-node3 1
                  array-elements3 2 int-lbl)

        (add-edge frame-elements frame-bind-idx
                  array-elements2 1 typedval-array-lbl)
        (add-edge literal-node2 1
                  array-elements2 2 int-lbl)

        (add-edge frame-elements frame-bind-idx
                  array-elements1 1 typedval-array-lbl)
        (add-edge literal-node1 1
                  array-elements1 2 int-lbl)

        (add-edge array-elements2 1
                  compound-node 1 typedval-lbl)
        (add-edge array-elements1 1
                  compound-node 2 typedval-lbl)
        (add-edge array-elements3 1
                  compound-node 3 typedval-lbl)

        (add-edge compound-node 1
                  0 1 typedval-lbl))))
