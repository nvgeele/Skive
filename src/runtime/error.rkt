#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide error)

(define error
  (let*-values
      ([(boundary) (make-graph-boundary "throw_error")]
       [(boundary lit1) (add-node boundary (make-literal-node "error"))])
    (add-edge boundary lit1 1 0 1 typedval-lbl)))
