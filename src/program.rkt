#!racket

(provide make-program
	 add-boundary
	 next-label)

(struct program (count boundaries)
	#:transparent)

(define (make-program)
  (program 0 '()))

(define (next-label program)
  (string->symbol (~a "proc" (program-count program))))

(define (add-boundary program boundary)
  (program (+ (program-count program) 1)
	   (cons boundary (program-boundaries program))))
