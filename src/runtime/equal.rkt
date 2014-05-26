#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide equal)

(define true-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary build) (add-node boundary (make-simple-node 143))]
       [(boundary lit) (add-node boundary (make-literal-node "true"))])
    (~> boundary
        (add-edge lit 1 build typedval-bool-idx bool-lbl)
        (add-edge build 1 0 1 typedval-lbl))))

(define false-boundary
  (let*-values
      ([(boundary) (make-graph-boundary "")]
       [(boundary build) (add-node boundary (make-simple-node 143))]
       [(boundary lit) (add-node boundary (make-literal-node "false"))])
    (~> boundary
        (add-edge lit 1 build typedval-bool-idx bool-lbl)
        (add-edge build 1 0 1 typedval-lbl))))

(define null-compound-node
  (make-tagcase `(,true-boundary ,false-boundary)
                #(0 1 1 1 1 1 1 1 1)))

(define int-compound-node
  (let*-values
      ([(int-boundary) (make-graph-boundary "")]
       [(int-boundary lbl1) (add-node int-boundary (make-simple-node 124))]
       [(int-boundary lbl2) (add-node int-boundary (make-simple-node 143))]
       [(int-boundary) (~> int-boundary
                           (add-edge 0 2 lbl1 1 int-lbl)
                           (add-edge 0 1 lbl1 2 int-lbl)
                           (add-edge lbl1 1 lbl2 typedval-bool-idx bool-lbl)
                           (add-edge lbl2 1 0 1 typedval-lbl))])
    (make-tagcase `(,int-boundary ,false-boundary)
                  #(1 0 1 1 1 1 1 1 1))))

(define float-compound-node
  (let*-values
      ([(int-boundary) (make-graph-boundary "")]
       [(int-boundary lbl1) (add-node int-boundary (make-simple-node 124))]
       [(int-boundary lbl2) (add-node int-boundary (make-simple-node 143))]
       [(int-boundary) (~> int-boundary
                           (add-edge 0 2 lbl1 1 float-lbl)
                           (add-edge 0 1 lbl1 2 float-lbl)
                           (add-edge lbl1 1 lbl2 typedval-bool-idx bool-lbl)
                           (add-edge lbl2 1 0 1 typedval-lbl))])
    (make-tagcase `(,int-boundary ,false-boundary)
                  #(1 1 0 1 1 1 1 1 1))))

(define bool-compound-node
  (let*-values
      ([(int-boundary) (make-graph-boundary "")]
       [(int-boundary lbl1) (add-node int-boundary (make-simple-node 124))]
       [(int-boundary lbl2) (add-node int-boundary (make-simple-node 143))]
       [(int-boundary) (~> int-boundary
                           (add-edge 0 2 lbl1 1 bool-lbl)
                           (add-edge 0 1 lbl1 2 bool-lbl)
                           (add-edge lbl1 1 lbl2 typedval-bool-idx bool-lbl)
                           (add-edge lbl2 1 0 1 typedval-lbl))])
    (make-tagcase `(,int-boundary ,false-boundary)
                  #(1 1 1 1 0 1 1 1 1))))

(define symbol-compound-node
  (let*-values
      ([(symb-bnd) (make-graph-boundary "")]
       [(symb-bnd lbl1) (add-node symb-bnd (make-simple-node 124))]
       [(symb-bnd lbl2) (add-node symb-bnd (make-simple-node 143))]
       [(symb-bnd) (~> symb-bnd
                       (add-edge 0 2 lbl1 1 int-lbl)
                       (add-edge 0 1 lbl1 2 int-lbl)
                       (add-edge lbl1 1 lbl2 typedval-bool-idx bool-lbl)
                       (add-edge lbl2 1 0 1 typedval-lbl))])
    (make-tagcase `(,symb-bnd ,false-boundary)
                  #(1 1 1 1 1 1 1 0 1))))

(define compound-node
  (let*-values
      ([(null-bnd) (make-graph-boundary "")]
       [(null-bnd lbl) (add-node null-bnd null-compound-node)]
       [(null-bnd) (~> null-bnd
                       (add-edge 0 2 lbl 1 typedval-lbl)
                       (add-edge lbl 1 0 1 typedval-lbl))]
       [(int-bnd) (make-graph-boundary "")]
       [(int-bnd lbl) (add-node int-bnd int-compound-node)]
       [(int-bnd) (~> int-bnd
                      (add-edge 0 2 lbl 1 typedval-lbl)
                      (add-edge 0 1 lbl 2 int-lbl)
                      (add-edge lbl 1 0 1 typedval-lbl))]
       [(float-bnd) (make-graph-boundary "")]
       [(float-bnd lbl) (add-node float-bnd float-compound-node)]
       [(float-bnd) (~> float-bnd
                        (add-edge 0 2 lbl 1 typedval-lbl)
                        (add-edge 0 1 lbl 2 float-lbl)
                        (add-edge lbl 1 0 1 typedval-lbl))]
       [(bool-bnd) (make-graph-boundary "")]
       [(bool-bnd lbl) (add-node bool-bnd bool-compound-node)]
       [(bool-bnd) (~> bool-bnd
                       (add-edge 0 2 lbl 1 typedval-lbl)
                       (add-edge 0 1 lbl 2 bool-lbl)
                       (add-edge lbl 1 0 1 typedval-lbl))]
       [(symb-bnd) (make-graph-boundary "")]
       [(symb-bnd lbl) (add-node symb-bnd symbol-compound-node)]
       [(symb-bnd) (~> symb-bnd
                       (add-edge 0 2 lbl 1 typedval-lbl)
                       (add-edge 0 1 lbl 2 int-lbl)
                       (add-edge lbl 1 0 1 typedval-lbl))])
    (make-tagcase `(,null-bnd
                    ,int-bnd
                    ,float-bnd
                    ,bool-bnd
                    ,false-boundary
                    ,symb-bnd)
                  #(0 1 2 4 3 4 4 5 4))))

(define equal
  (let*-values
      ([(boundary) (make-graph-boundary "equal")]
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
