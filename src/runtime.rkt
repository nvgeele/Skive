#lang racket

(require "typing.rkt")

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
