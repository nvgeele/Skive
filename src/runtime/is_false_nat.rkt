#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide is_false_nat)


(define compound-node
  (let*-values
      ([(bool-bnd) (make-graph-boundary "")]
       [(bool-bnd lbl1) (add-node bool-bnd (make-simple-node 139))]
       [(bool-bnd) (~> bool-bnd
                       (add-edge 0 1 lbl1 1 bool-lbl)
                       (add-edge lbl1 1 0 1 bool-lbl))]
       [(false-bnd) (make-graph-boundary "")]
       [(false-bnd lit) (add-node false-boundary (make-literal-node "false"))]
       [(false-bnd) (add-edge false-bnd lit 1 0 1 bool-lbl)])
    (make-tagcase `(,bool-bnd ,false-bnd)
                  #(1 1 1 1 0 1 1 1 1))))


(define is_false_nat
  (let*-values
      ([(bnd) (make-graph-boundary "is_false_nat" #f is-false-nat-func-lbl)]
       [(bnd lbl1) (add-node bnd compound-node)])
    (~> bnd
        (add-edge 0 1 lbl1 1 typedval-lbl)
        (add-edge lbl1 1 0 1 bool-lbl))))
