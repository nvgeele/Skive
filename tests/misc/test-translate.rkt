#lang racket

(require "../../src/expand.rkt")
(require "../../src/analyse.rkt")
(require "../../src/generate.rkt")
(require "../../src/program.rkt")
(require "../../src/translate.rkt")
(require "../../src/graph-boundary.rkt")

;; Works
(define prog1 '1)

;; Works
(define prog2 '((lambda (a) a) 1))

;; Works
(define prog3
  '((let ((a 1))
      (let ((b 2)
	    (c 3))
	(lambda () (+ a b c))))))

;; Works
(define prog4 '(cons 1 2))

;; Works
(define prog5
  '(let ((a (lambda (f) (f 1 2)))
	 (f (lambda (a b) (+ a b))))
     (a f)))

;; Works
(define prog6
  '(let ((a (lambda (f) (f 1 2)))
	 (f (lambda (a b) (cons a b))))
     1))

;; Works
(define prog7 '(cdr (cons (+ 1 2) (+ 3 4))))

;; Works
(define prog8
  '(let ((apply-twice (lambda (f) (lambda (x) (f (f x)))))
	 (square (lambda (x) (* x x))))
     (let ((omg (apply-twice square)))
       (omg 2))))

;; Works
(define prog9
  '(let ((i (lambda (x) x)))
     1))

;; Works
(define prog10
  '(let ((i (lambda (x) (* x x))))
     (let ((x 5)
	   (y 4))
       (i y))))

;; Works
(define prog11
  '(let ((a (lambda (x) x)))
     (let ((b (lambda (x) x)))
       80085)))

;; Segfaults so I just presume it runs out of memory
(define prog12
  '(let ((y (lambda (f)
	      ((lambda (x) (x x))
	       (lambda (x) (f (lambda (y) ((x x) y)))))))
	 (almost-endlos (lambda (f) (lambda (v) (f v)))))
     (let ((endlos (y almost-endlos)))
       (endlos 0))))

(define (do-test prog [print #f] [graphviz #f])
  (let* ((expanded (expand prog))
	 (analysed (analyse expanded))
	 (generated (generate analysed))
	 (translated (translate generated))
	 (output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (when print
      (display (~a "Original:\n" prog
		   "\n\nExpanded:\n" expanded
		   "\n\nAnalysed:\n" analysed
		   "\n\nGenerated:\n" generated)))
    (display translated output)
    (close-output-port output)
    (when graphviz
      (for ((gb (program-boundaries generated)))
	   (display (topological-sort gb))
	   (newline)(newline)
	   (display (graph-boundary->dot-file gb))))
    (display "done!\n")))
