#lang racket

(require "../graph-boundary.rkt"
         "../node.rkt"
         "../compound-node.rkt"
         "../threading.rkt"
         "../typing.rkt")

(provide is_list_intern)

(define is_list_intern
  (let*-values
      ([(recur-boundary) (make-boundary "")]
       [(recur-boundary cdr) (add-node recur-boundary (make-simple-node 144))]
       [(recur-boundary call) (add-node recur-boundary (make-simple-node 120))]
       [(recur-boundary func) (add-node recur-boundary (make-literal-node "is_list_intern"))]
       [(recur-boundary) (~> recur-boundary
                             (add-edge 0 1 cdr 1 conscell-lbl)
                             (add-edge func 1 call 1 typedval-typedval-function-lbl)
                             (add-edge cdr conscell-cdr-idx call 2 typedval-lbl)
                             (add-edge call 1 0 1 typedval-lbl))]
       [(null-boundary) (make-boundary "")] ;; Returns true
       [(null-boundary true) (add-node null-boundary (make-literal-node "true"))]
       [(null-boundary build) (add-node null-boundary (make-simple-node 143))]
       [(null-boundary) (~> null-boundary
                            (add-edge true 1 build typedval-bool-idx bool-lbl)
                            (add-edge build 1 0 1 typedval-lbl))]
       [(false-boundary) (make-boundary "")] ;; Returns true
       [(false-boundary true) (add-node false-boundary (make-literal-node "false"))]
       [(false-boundary build) (add-node false-boundary (make-simple-node 143))]
       [(false-boundary) (~> false-boundary
                            (add-edge true 1 build typedval-bool-idx bool-lbl)
                            (add-edge build 1 0 1 typedval-lbl))]
       [(compound-node) (make-tagcase
                         `(,recur-boundary ,null-boundary ,false-boundary)
                         #(1 2 2 2 2 0 2 2 2))]
       [(boundary) (make-boundary "is_list_intern" #f typedval-typedval-function-lbl)]
       [(boundary lbl) (add-node boundary compound-node)])
    (~> boundary
        (add-edge 0 1 lbl 1 typedval-lbl)
        (add-edge lbl 1 0 1 typedval-lbl))))
