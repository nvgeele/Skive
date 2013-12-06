#lang racket

(require "typing.rkt")

(provide generate-runtime-code)

(define is_num
"G	35	\"is_num\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 0 0 1 0 0 0 0	
E	2 1	3 1	10	 
{ Compound   4   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 4 2 7 0 1 0 0 0 0 0	
E	2 1	4 1	10	 
N 5	141	
E	4 1	5 1	1	
E	3 1	5 2	1	
N 6	143
E	5 1	6 5	1	
E	6 1	0 1	10\n")

(define is_int
"G	35	\"is_int\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 0 1 0 0 0 0 0	
E	2 1	3 1	10	 
N 4	143
E	3 1	4 5	1	
E	4 1	0 1	10\n")

(define is_float
"G	35	\"is_float\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 0 0 1 0 0 0 0	
E	2 1	3 1	10	 
N 4	143
E	3 1	4 5	1	
E	4 1	0 1	10\n")

(define is_null
"G	35	\"is_null\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 1 0 0 0 0 0 0	
E	2 1	3 1	10	 
N 4	143
E	3 1	4 5	1	
E	4 1	0 1	10\n")

(define is_string
"G	35	\"is_string\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 0 0 0 1 0 0 0	
E	2 1	3 1	10	 
N 4	143
E	3 1	4 5	1	
E	4 1	0 1	10\n")

(define is_bool
"G	35	\"is_bool\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 0 0 0 0 1 0 0	
E	2 1	3 1	10	 
N 4	143
E	3 1	4 5	1	
E	4 1	0 1	10\n")

(define is_cons
"G	35	\"is_cons\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
L		0 1	1 \"false\"	
G	0	
L		0 1	1 \"true\"	
G	0	
L		0 1	1 \"error\"	
} 3 2 7 0 0 0 0 0 1 0	
E	2 1	3 1	10	 
N 4	143
E	3 1	4 5	1	
E	4 1	0 1	10\n")

(define is_false
"G	35	\"is_false\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
N 1	139	
E	0 1	1 1	1	 
N 2	143
E	1 1	2 5	1	
E	2 1	0 1	10	
G	0	
N 1	143
L		1 5	1 \"false\"	
E	1 1	0 1	10	
} 3 2 7 1 1 1 1 0 1 1	
E	2 1	3 1	10	 
E	3 1	0 1	10\n")

(define rt-cons
"G	35	\"cons\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"1\"	
N 3	105	
E	1 2	3 1	26	 
L		3 2	4 \"0\"	
N 4	143	
E	3 1	4 1	10	 
E	2 1	4 2	10	 
N 5	143
E	4 1	5 6	18	 
E	5 1	0 1	10\n")

(define get_car
"G	35	\"get_car\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
N 1	144	
E	0 1	1 1	18	 
E	1 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 3 2 7 1 1 1 1 1 0 1	
E	2 1	3 1	10	 
E	3 1	0 1	10\n")

(define get_cdr
"G	35	\"get_cdr\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"0\"	
{ Compound   3   2
G	0	
N 1	144	
E	0 1	1 1	18	 
E	1 2	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 3 2 7 1 1 1 1 1 0 1	
E	2 1	3 1	10	 
E	3 1	0 1	10\n")

(define plus
"G	35	\"plus\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"1\"	
N 3	105	
E	1 2	3 1	26	 
L		3 2	4 \"0\"	
{ Compound   4   2
G	0	
{ Compound   1   2
G	0	
N 1	141	
E	0 2	1 1	4	 
E	0 1	1 2	4	 
N 2	143
E	1 1	2 2	4	 
E	2 1	0 1	10	
G	0	
N 1	123	
E	0 2	1 1	4	 
N 2	141	
E	1 1	2 1	3	 
E	0 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	4	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	123	
E	0 1	1 1	4	 
N 2	141	
E	0 2	2 1	3	 
E	1 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
N 1	141	
E	0 2	1 1	3	 
E	0 1	1 2	3	 
N 2	143
E	1 1	2 3	3	 
E	2 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	3	 
E	1 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 4 2 7 2 0 1 2 2 2 2	
E	3 1	4 1	10	 
E	2 1	4 2	10	 
E	4 1	0 1	10\n")

(define minus
"G	35	\"minus\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"1\"	
N 3	105	
E	1 2	3 1	26	 
L		3 2	4 \"0\"	
{ Compound   4   2
G	0	
{ Compound   1   2
G	0	
N 1	135	
E	0 2	1 1	4	 
E	0 1	1 2	4	 
N 2	143
E	1 1	2 2	4	 
E	2 1	0 1	10	
G	0	
N 1	123	
E	0 2	1 1	4	 
N 2	135	
E	1 1	2 1	3	 
E	0 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	4	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	123	
E	0 1	1 1	4	 
N 2	135	
E	0 2	2 1	3	 
E	1 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
N 1	135	
E	0 2	1 1	3	 
E	0 1	1 2	3	 
N 2	143
E	1 1	2 3	3	 
E	2 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	3	 
E	1 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 4 2 7 2 0 1 2 2 2 2	
E	3 1	4 1	10	 
E	2 1	4 2	10	 
E	4 1	0 1	10\n")

(define multiply
"G	35	\"multiply\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"1\"	
N 3	105	
E	1 2	3 1	26	 
L		3 2	4 \"0\"	
{ Compound   4   2
G	0	
{ Compound   1   2
G	0	
N 1	152	
E	0 2	1 1	4	 
E	0 1	1 2	4	 
N 2	143
E	1 1	2 2	4	 
E	2 1	0 1	10	
G	0	
N 1	123	
E	0 2	1 1	4	 
N 2	152	
E	1 1	2 1	3	 
E	0 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	4	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	123	
E	0 1	1 1	4	 
N 2	152	
E	0 2	2 1	3	 
E	1 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
N 1	152	
E	0 2	1 1	3	 
E	0 1	1 2	3	 
N 2	143
E	1 1	2 3	3	 
E	2 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	3	 
E	1 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 4 2 7 2 0 1 2 2 2 2	
E	3 1	4 1	10	 
E	2 1	4 2	10	 
E	4 1	0 1	10\n")

(define divide
"G	35	\"divide\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"1\"	
N 3	105	
E	1 2	3 1	26	 
L		3 2	4 \"0\"	
{ Compound   4   2
G	0	
{ Compound   1   2
G	0	
N 1	122	
E	0 2	1 1	4	 
E	0 1	1 2	4	 
N 2	143
E	1 1	2 2	4	 
E	2 1	0 1	10	
G	0	
N 1	123	
E	0 2	1 1	4	 
N 2	122	
E	1 1	2 1	3	 
E	0 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	4	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	123	
E	0 1	1 1	4	 
N 2	122	
E	0 2	2 1	3	 
E	1 1	2 2	3	 
N 3	143
E	2 1	3 3	3	 
E	3 1	0 1	10	
G	0	
N 1	122	
E	0 2	1 1	3	 
E	0 1	1 2	3	 
N 2	143
E	1 1	2 3	3	 
E	2 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 1 2 7 2 0 1 2 2 2 2	
E	0 2	1 1	10	 
E	0 1	1 2	3	 
E	1 1	0 1	10	
G	0	
L		0 1	10 \"error\"	
} 4 2 7 2 0 1 2 2 2 2	
E	3 1	4 1	10	 
E	2 1	4 2	10	 
E	4 1	0 1	10\n")

(define equal
"G	35	\"equal\"	
N 1	144	
E	0 1	1 1	27	 
N 2	105	
E	1 2	2 1	26	 
L		2 2	4 \"1\"	
N 3	105	
E	1 2	3 1	26	 
L		3 2	4 \"0\"	
{ Compound   4   2
G	0	
{ Compound   1   2
G	0	
N 1	143
L		1 5	1 \"true\"	
E	1 1	0 1	10	
G	0	
N 1	143
L		1 5	1 \"false\"	
E	1 1	0 1	10	
} 1 2 7 0 1 1 1 1 1 1	
E	0 2	1 1	10	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	124	
E	0 2	1 1	4	 
E	0 1	1 2	4	 
N 2	143
E	1 1	2 5	1	
E	2 1	0 1	10	
G	0	
N 1	143
L		1 5	1 \"false\"	
E	1 1	0 1	10	
} 1 2 7 1 0 1 1 1 1 1	
E	0 2	1 1	10	 
E	0 1	1 2	4	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	124	
E	0 2	1 1	3	 
E	0 1	1 2	3	 
N 2	143
E	1 1	2 5	1	
E	2 1	0 1	10	
G	0	
N 1	143
L		1 5	1 \"false\"	
E	1 1	0 1	10	
} 1 2 7 1 1 0 1 1 1 1	
E	0 2	1 1	10	 
E	0 1	1 2	3	 
E	1 1	0 1	10	
G	0	
{ Compound   1   2
G	0	
N 1	124	
E	0 2	1 1	1	 
E	0 1	1 2	1	 
N 2	143
E	1 1	2 5	1	
E	2 1	0 1	10	
G	0	
N 1	143
L		1 5	1 \"false\"	
E	1 1	0 1	10	
} 1 2 7 1 1 1 1 0 1 1	
E	0 2	1 1	10	 
E	0 1	1 2	1	 
E	1 1	0 1	10	
G	0	
N 1	143
L		1 5	1 \"false\"	
E	1 1	0 1	10	
} 4 2 7 0 1 2 4 3 4 4	
E	3 1	4 1	10	 
E	2 1	4 2	10	 
E	4 1	0 1	10	\n")

(define is_false_nat
"G	39	\"is_false_nat\"	
{ Compound   1   2
G	0	
N 1	139	
E	0 1	1 1	1	 
E	1 1	0 1	1	
G	0	
L		0 1	1 \"false\"	
} 1 2 7 1 1 1 1 0 1 1	
E	0 1	1 1	10	 
E	1 1	0 1	1\n")

(define (generate-runtime-code)
  (string-append ;is_num
		 ;is_int
		 ;is_float
		 ;is_null
		 ;is_string
		 ;is_bool 
		 ;is_cons
		 ;is_false
		 get_car
		 get_cdr
		 rt-cons
		 plus
		 minus
		 multiply
		 divide
		 equal
		 is_false_nat
		 ))
