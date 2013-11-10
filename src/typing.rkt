#lang racket

(provide bool-lbl char-lbl float-lbl int-lbl
	 null-lbl string-lbl
	 typedval-lbl
	 typedval-null-idx typedval-int-idx typedval-float-idx
	 typedval-string-idx typedval-bool-idx typedval-cons-idx
	 conscell-lbl
	 conscell-car-idx conscell-cdr-idx
	 type-check-fun-lbl
	 binary-typedval-fun-lbl
	 generate-type-definitions-code)

(struct type-table (table)
	#:transparent)

(define (make-type-table)
  (type-table (hash)))

(define table type-table-table)

(define bool-lbl   1)
(define char-lbl   2)
(define float-lbl  3)
(define int-lbl    4)
(define null-lbl   5)
(define string-lbl 6)

(define typedval-lbl        7)
(define typedval-null-idx   1)
(define typedval-int-idx    2)
(define typedval-float-idx  3)
(define typedval-string-idx 4)
(define typedval-bool-idx   5)
(define typedval-cons-idx   6)

(define conscell-lbl (+ typedval-lbl 7))
(define conscell-car-idx 1)
(define conscell-cdr-idx 2)

(define single-typedval-tuple-lbl (+ conscell-lbl 2))
(define single-bool-tuple-lbl (+ single-typedval-tuple-lbl 1))
(define double-typedval-tuple-lbl (+ single-bool-tuple-lbl 1))

(define type-check-fun-lbl (+ double-typedval-tuple-lbl 1))
(define binary-typedval-fun-lbl (+ type-check-fun-lbl 1))

(define (make-basic-type-definition label basic-type)
  (format "T ~s 1 ~s\n" label basic-type))

(define (make-array-definition label type)
  (format "T ~s 0 ~s\n" label type))

(define (make-record-definition label field1 . fieldn)
  (string-append
    (format "T ~s 5 ~s\n"
	    label (+ label 1))
    (let loop ((cur field1) (rem fieldn)
	       (lbl (+ 1 label)) (res ""))
      (if (null? rem)
	(string-append res (format "T ~s 2 ~s 0\n" lbl cur))
	(loop (car rem) (cdr rem) (+ lbl 1)
	      (string-append res (format "T ~s 2 ~s ~s\n" lbl cur (+ lbl 1))))))))

(define (make-tuple-definition label type1 . typen)
  (if (null? typen)
    (format "T ~s 8 ~s 0\n" label type1)
    (string-append
      (format "T ~s 8 ~s ~s\n"
	      label type1 (+ 1 label))
      (apply make-tuple-definition
	     (+ 1 label) (car typen) (cdr typen)))))

(define (extend-tuple-definition label tuple type)
  (format "T ~s 8 ~s ~s\n" label type tuple))

(define (make-union-definition label type1 . typen)
  (string-append
    (format "T ~s 9 ~s\n" label (+ label 1))
    (let loop ((cur type1) (rem typen)
	       (lbl (+ 1 label)) (res ""))
      (if (null? rem)
	(string-append res (format "T ~s 7 ~s 0\n" lbl cur))
	(loop (car rem) (cdr rem) (+ 1 lbl)
	      (string-append res (format "T ~s 7 ~s ~s\n" lbl cur (+ lbl 1))))))))

(define (make-function-definition label in-lbl out-lbl)
  (format "T ~s 3 ~s ~s\n" label in-lbl out-lbl))

(define (generate-type-definitions-code); type-table)
  (string-append
    (make-basic-type-definition bool-lbl  0)
    (make-basic-type-definition char-lbl  1)
    (make-basic-type-definition float-lbl 2)
    (make-basic-type-definition int-lbl   3)
    (make-basic-type-definition null-lbl  4)

    (make-array-definition string-lbl char-lbl)

    (make-union-definition typedval-lbl
			   null-lbl int-lbl
			   float-lbl string-lbl
			   bool-lbl conscell-lbl)

    (make-record-definition conscell-lbl typedval-lbl typedval-lbl)

    (make-tuple-definition single-typedval-tuple-lbl typedval-lbl)
    (make-tuple-definition single-bool-tuple-lbl bool-lbl)

    (extend-tuple-definition double-typedval-tuple-lbl single-typedval-tuple-lbl typedval-lbl)

    (make-function-definition type-check-fun-lbl
			      single-typedval-tuple-lbl
			      single-bool-tuple-lbl)
    (make-function-definition binary-typedval-fun-lbl
			      double-typedval-tuple-lbl
			      single-typedval-tuple-lbl)))
