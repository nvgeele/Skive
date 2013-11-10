#lang racket

(require "typing.rkt")

(provide generate-runtime-code)

(define is_num
  (~a
"G	" type-check-fun-lbl "	\"is_num\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 0 1 0 0 0 0
E	0 1	1 1	" typedval-lbl "
{ Compound   2   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 2 2 6 0 0 1 0 0 0
E	0 1	2 1	" typedval-lbl "
N 3	141
E	1 1	3 1	" bool-lbl "
E	2 1	3 2	" bool-lbl "
E	3 1	0 1	" bool-lbl "\n"))

(define is_int
  (~a
"G	" type-check-fun-lbl "	\"is_int\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 0 1 0 0 0 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define is_float
  (~a
"G	" type-check-fun-lbl "	\"is_float\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 0 0 1 0 0 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define is_null
  (~a
"G	" type-check-fun-lbl "	\"is_null\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 1 0 0 0 0 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define is_string
  (~a
"G	" type-check-fun-lbl "	\"is_string\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 0 0 0 1 0 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define is_bool
  (~a
"G	" type-check-fun-lbl "	\"is_bool\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 0 0 0 0 1 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define is_cons
  (~a
"G	" type-check-fun-lbl "	\"is_cons\"
{ Compound   1   2
G	0
L		0 1	" bool-lbl " \"false\"
G	0
L		0 1	" bool-lbl " \"true\"
G	0
L		0 1	" bool-lbl " \"error\"
} 1 2 6 0 0 0 0 0 1
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define is_false
  (~a
"G	" type-check-fun-lbl "	\"is_false\"
{ Compound   1   2
G	0
N 1	139
E	0 1	1 1	" bool-lbl "
E	1 1	0 1	" bool-lbl "
G	0
L		0 1	" bool-lbl " \"false\"
} 1 2 6 1 1 1 1 0 1
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" bool-lbl "\n"))

(define get_car
  (~a
"G	" unary-typedval-fun-lbl "	\"get_car\"
{ Compound   1   2
G	0
N 1	144
E	0 1	1 1	" conscell-lbl "
E	1 1	0 1	" typedval-lbl "
G	0
L		0 1	" typedval-lbl " \"error\"
} 1 2 6 1 1 1 1 1 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" typedval-lbl "\n"))

(define get_cdr
  (~a
"G	" unary-typedval-fun-lbl "	\"get_cdr\"
{ Compound   1   2
G	0
N 1	144
E	0 1	1 1	" conscell-lbl "
E	1 2	0 1	" typedval-lbl "
G	0
L		0 1	" typedval-lbl " \"error\"
} 1 2 6 1 1 1 1 1 0
E	0 1	1 1	" typedval-lbl "
E	1 1	0 1	" typedval-lbl "\n"))

(define cons
  (~a
"G	" binary-typedval-fun-lbl "	\"cons\"
N 1	143
E	0 1	1 1	" typedval-lbl "
E	0 2	1 2	" typedval-lbl "
N 2	143
E	1 1	2 6	" conscell-lbl "
E	2 1	0 1	" typedval-lbl "\n"))

(define plus
  (~a
"G	" binary-typedval-fun-lbl "	\"plus\"
{ Compound   1   2
G	0
{ Compound   1   2
G	0
N 1	141
E	0 2	1 1	" int-lbl "
E	0 1	1 2	" int-lbl "
N 2	143
E	1 1	2 2	" int-lbl "
E	2 1	0 1	" typedval-lbl "
G	0
N 1	123
E	0 2	1 1	" int-lbl "
N 2	141
E	1 1	2 1	" float-lbl "
E	0 1	2 2	" float-lbl "
N 3	143
E	2 1	3 3	" float-lbl "
E	3 1	0 1	" typedval-lbl "
G	0
L		0 1	" typedval-lbl " \"error\"
} 1 2 6 2 0 1 2 2 2
E	0 2	1 1	" typedval-lbl "
E	0 1	1 2	" int-lbl "
E	1 1	0 1	" typedval-lbl "
G	0
{ Compound   1   2
G	0
N 1	123
E	0 1	1 1	" int-lbl "
N 2	141
E	0 2	2 1	" float-lbl "
E	1 1	2 2	" float-lbl "
N 3	143
E	2 1	3 3	" float-lbl "
E	3 1	0 1	" typedval-lbl "
G	0
N 1	141
E	0 2	1 1	" float-lbl "
E	0 1	1 2	" float-lbl "
N 2	143
E	1 1	2 3	" float-lbl "
E	2 1	0 1	" typedval-lbl "
G	0
L		0 1	" typedval-lbl " \"error\"
} 1 2 6 2 0 1 2 2 2
E	0 2	1 1	" typedval-lbl "
E	0 1	1 2	" float-lbl "
E	1 1	0 1	" typedval-lbl "
G	0
L		0 1	" typedval-lbl " \"error\"
} 1 2 6 2 0 1 2 2 2
E	0 1	1 1	" typedval-lbl "
E	0 2	1 2	" typedval-lbl "
E	1 1	0 1	" typedval-lbl "\n"))

(define (generate-runtime-code)
  (string-append is_num is_int is_float
		 is_null is_string is_bool 
		 is_cons is_false get_car
		 get_cdr cons plus))
