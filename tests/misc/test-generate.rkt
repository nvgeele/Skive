#lang racket

(require "../../src/expand.rkt")
(require "../../src/analyse.rkt")
(require "../../src/generate.rkt")
(require "../../src/program.rkt")

(define prog1 '((lambda (a) a) 1))

(define prog2
  '((let ((a 1))
      (let ((b 2)
	    (c 3))
	(lambda () (+ a b c))))))

(let* ((expanded (expand prog1))
       (analysed (analyse expanded))
       (generated (generate analysed)))
  (display (~a "Expanded:\n" expanded
	       "\n\nAnalysed:\n" analysed
	       "\n\nGenerated:\n" generated)))
;"\n\n\nDOT:\n" (program->dot-file program)))))
