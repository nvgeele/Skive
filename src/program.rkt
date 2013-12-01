#!racket

(require "natives.rkt")

(provide make-program
	 add-boundary
	 next-label)

(struct program (count boundaries)
	#:transparent)

(define (make-program)
  (program (hash-count natives) '()))

(define (next-label-str program)
  (~a "proc" (program-count program)))

(define (next-label program)
  (program-count program))

(define (add-boundary prog boundary)
  (program (+ (program-count prog) 1)
	   (cons boundary (program-boundaries prog))))
