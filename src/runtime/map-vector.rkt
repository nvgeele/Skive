#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide map-vector)

(define error-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary lit) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit 1 0 1 typedval-lbl)))

;; 0 - 1 : vector//array
;; 0 - 2 : function integer
;; 0 - 3 : procedure environment in back
(define compound-node-loop
  (let*-values
      ([(generator) (make-graph-boundary "")]
       [(generator scatter) (add-node generator (make-simple-node 114))]
       [(generator) (~> generator
                        (add-edge 0 1 scatter 1 typedval-array-lbl)
                        (add-edge scatter 1 0 4 multiple-typedval-lbl)
                        (add-edge scatter 2 0 5 multiple-int-lbl))]
       [(body) (make-graph-boundary "")]
       [(body tvbld) (add-node body (make-simple-node 143))]
       [(body lit1) (add-node body (make-literal-node 0))]
       [(body array-build) (add-node body (make-simple-node 103))]
       [(body frame-build) (add-node body (make-simple-node 143))]
       [(body lit2) (add-node body (make-literal-node "call"))]
       [(body call) (add-node body (make-simple-node 120))]
       [(body) (~> body
                   (add-edge 0 5 tvbld typedval-int-idx int-lbl)
                   (add-edge lit1 1 array-build 1 int-lbl)
                   (add-edge tvbld 1 array-build 2 typedval-lbl)
                   (add-edge 0 4 array-build 3 typedval-lbl)
                   (add-edge 0 3 frame-build frame-prev-idx back-lbl)
                   (add-edge array-build 1 frame-build frame-bind-idx typedval-array-lbl)
                   (add-edge lit2 1 call 1 call-function-lbl)
                   (add-edge 0 2 call 2 int-lbl)
                   (add-edge frame-build 1 call 3 frame-lbl)
                   (add-edge call 1 0 6 typedval-lbl))]
       [(results) (make-graph-boundary "")]
       ;; NOTE: we do not need the FirstValue node, apparently,
       ;; it isn't even implemented yet...
       ;; So, we apparently need to do the thing with the literal node.
       ;; [(results first-val) (add-node results (make-simple-node 126))]
       [(results lit) (add-node results (make-literal-node 0))]
       [(results gather) (add-node results (make-simple-node 107))]
       [(results) (~> results
                      ;;(add-edge 0 5 first-val 1 multiple-int-lbl)
                      ;;(add-edge first-val 1 gather 1 int-lbl)
                      (add-edge lit 1 gather 1 int-lbl)
                      (add-edge 0 6 gather 2 multiple-typedval-lbl)
                      (add-edge gather 1 0 1 typedval-array-lbl))])
    (make-for generator body results)))

;; 0 - 1 : The vector typedval
;; 0 - 2 : The procedure typedval
;; TODO: check amount of arguments etc
(define compound-node-vector
  (let*-values
      ([(vb) (make-graph-boundary "")] ;; vb = vector-boundary
       [(vb vect-elements) (add-node vb (make-simple-node 144))]
       [(vb closure-elements) (add-node vb (make-simple-node 144))]
       [(vb back-build) (add-node vb (make-simple-node 143))]
       [(vb loop) (add-node vb compound-node-loop)]
       [(vb vect-build) (add-node vb (make-simple-node 143))]
       [(vb tval-build) (add-node vb (make-simple-node 143))]
       [(vb) (~> vb
                 (add-edge 0 1 vect-elements 1 vector-lbl)
                 (add-edge 0 2 closure-elements 1 closure-lbl)
                 (add-edge closure-elements closure-env-idx back-build back-frame-idx frame-lbl)
                 (add-edge vect-elements vector-content-idx loop 1 typedval-array-lbl)
                 (add-edge closure-elements closure-func-idx loop 2 int-lbl)
                 (add-edge back-build 1 loop 3 back-lbl)
                 (add-edge loop 1 vect-build vector-content-idx typedval-array-lbl)
                 (add-edge vect-elements vector-size-idx vect-build vector-size-idx int-lbl)
                 (add-edge vect-build 1 tval-build typedval-vect-idx vector-lbl)
                 (add-edge tval-build 1 0 1 typedval-lbl))])
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

;; TODO: Possible enhancement for all natives: type checker generators.
;; As in: (make-type-check typedval-type-idx boundary-to-do-if-ok error-boundary)
(define map-vector
  (let*-values
      ([(boundary) (make-graph-boundary "map_vector")]
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
