#!racket

(provide make-program
	 add-boundary
	 next-label)

(struct program (count boundaries)
	#:transparent)

(define (make-program)
  (program 0 '()))

(define (next-label program)
  (~a "proc" (program-count program)))

(define (add-boundary prog boundary)
  (program (+ (program-count prog) 1)
	   (cons boundary (program-boundaries prog))))
