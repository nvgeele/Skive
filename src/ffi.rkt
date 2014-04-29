#!racket

(require ffi/unsafe
	 ffi/unsafe/define
	 "typing.rkt")

(provide make-thunk
         parse parse-fibre-input scan-fibre) ;; TODO: Remove, export for debugging only!

;;;; Depends on OS/architecture(?)
(define O_NONBLOCK 4)
(define F_SETFL    4)

(define-ffi-definer define-cl (ffi-lib #f))
(define _file_ptr (_cpointer _file))

(define-cl fdopen (_fun _int _string -> _file_ptr))
(define-cl fclose (_fun _file_ptr -> _int))
(define-cl fgetc (_fun _file_ptr -> _int))
(define-cl fflush (_fun _file_ptr -> _int))
(define-cl pipe (_fun _pointer -> _int))

(define-cl fileno (_fun _file_ptr -> _int))
(define-cl fcntl (_fun _int _int _int -> _int))

(define (make-cpipe)
  (let* ((pfds (malloc (* 2 (ctype-sizeof _int)) _int))
	 (res (pipe pfds)))
    (if (= -1 res)
        (error "Could not make pipe")
        (values (ptr-ref pfds _int 0) ;input as in read from here
                (ptr-ref pfds _int 1))))) ;output as in write to here

(define (read-from-fs fs)
  (let read-output ((res (fgetc fs))
		    (out '()))
    (if (= res -1)
        #|(read (open-input-bytes
        (list->bytes (reverse out))))|#
        (bytes->string/utf-8 (list->bytes (reverse out)))
        (read-output (fgetc fs) (cons res out)))))

(define (read-until input delim)
  (let loop ((res '()))
    (let ((char (read-char input)))
      (cond ((or (eof-object? char)
		 (char=? char delim))
	     (list->string (reverse res)))
	    ((char-whitespace? char) (loop res))
	    (else (loop (cons char res)))))))

(define (scan-number input c)
  (let loop ((chars `(,c)))
    (let ((c (peek-char input)))
      (if (char-numeric? c)
          (loop (cons (read-char input) chars))
          ;; TODO: Can't this be written a bit, well, better?
          (string->number (list->string (reverse chars)))))))

(define (scan-string input)
  (let loop ((chars '()))
    (let ((c (read-char input)))
      (cond ((char=? c #\\)
             (loop (cons (read-char input)
                         (cons c chars))))
            ((char=? c #\")
             (list->string (reverse chars)))
            (else
             (loop (cons c chars)))))))

(define (scan-fibre str)
  (let loop ((input (open-input-string str))
             (tokens '()))
    (let ((c (read-char input)))
      (cond ((eof-object? c)
             (reverse tokens))
            ((char-whitespace? c)
             (loop input tokens))
            ((char=? c #\u28) ;; (
             (loop input (cons '(lpar) tokens)))
            ((char=? c #\u29) ;; )
             (loop input (cons '(rpar) tokens)))
            ((char=? c #\u3c) ;; <
             (loop input (cons '(st) tokens)))
            ((char=? c #\u3e) ;; >
             (loop input (cons '(gt) tokens)))
            ((char=? c #\u3a) ;; :
             (loop input (cons '(col) tokens)))
            ((char=? c #\[)
             (loop input (cons '(lbc) tokens)))
            ((char=? c #\])
             (loop input (cons '(rbc) tokens)))
            ((char=? c #\,)
             (loop input (cons '(com) tokens)))
            ((char-numeric? c)
             (loop input (cons `(num ,(scan-number input c)) tokens)))
            ((char=? c #\") ;; double quote
             (loop input (cons `(string ,(scan-string input)) tokens)))
            ((char=? c #\u23) ;; #
             (read-line input) ;; drop all the rubbish
             (loop input tokens))
            ((char=? c #\n) ;; nil (literal)
             (read-char input) ;; read #\i
             (read-char input) ;; read #\l
             (loop input (cons '(nil) tokens)))
            ((char=? c #\T) ;; literal true
             (loop input (cons '(bool #t) tokens)))
            ((char=? c #\F) ;; literal false
             (loop input (cons '(bool #f) tokens)))))))

(define (parse-bindings tokens)
  (match tokens
    [(list-rest '(lbc) (list 'num n1) '(com) (list 'num n2) '(col) rest)
     (cdr (foldl (lambda (i tokens)
                   (match tokens
                     [(list-rest '(lpar) (list 'num n) '(col) rest)
                      (let-values ([(ignore tokens)
                                    (parse-typedval n rest)])
                        tokens)]))
                 rest
                 (range 0 (+ (- n2 n1) 1))))]))

(define (parse-environment tokens)
  (match tokens
    [(list-rest '(st) '(lpar) (list 'num 1) '(col) '(nil) '(rpar) rest)
     (let ((tokens (parse-bindings rest)))
       (cdr tokens))]
    [(list-rest '(st) '(lpar) (list 'num 0) '(col) rest)
     (let ((tokens (parse-environment rest)))
       (cdr (parse-bindings (cdr tokens))))]
    [_ (error "Incorrect input -- parse-environment")]))

(define (parse-typedval type tokens)
  (case type
    [(0) ;; null
     (match tokens
       [(list-rest '(nil) '(rpar) rest)
        (values null rest)]
       [_ (error "Incorrect input -- parse null")])]
    [(1) ;; integer
     (match tokens
       [(list-rest (list 'num n) '(rpar) rest)
        (values n rest)]
       [_ (error "Incorrect input -- parse int")])]
    [(2)
     (error "2 is unknown")]
    [(3) ;; string
     (match tokens
       [(list-rest (list 'string s) '(rpar) rest)
        (values s rest)]
       [_ (error "Incorrect input -- parse string")])]
    [(4) ;; Bool
     (match tokens
       [(list-rest (list 'bool b) '(rpar) rest)
        (values b rest)]
       [_ (error "Incorrect input -- parse bool")])]
    [(5) ;; cons
     (match tokens
       [(list-rest '(st) '(lpar) (list 'num n) '(col) rest)
        (let-values ([(car tokens) (parse-typedval n rest)])
          (match tokens
            [(list-rest '(lpar) (list 'num n) '(col) rest)
             (let-values ([(cdr tokens) (parse-typedval n rest)])
               ;; cddr because we need to pop off 'gt and 'rpar
               (values (cons car cdr) (cddr tokens)))]
            [_ (display tokens)(error "Incorrect input -- parse cons 1")]))]
       [_ (error "Incorrect input -- parse cons 2")])]
    [(6) ;; closure
     (match tokens
       [(list-rest '(st) (list num n1) (list num n2) (list num n3) rest)
        (let ((tokens (parse-environment rest)))
          ;; Due to technical reasons, we will just return the keyword #:closure
          ;; instead of an actual usable closure... We will also parse the whole
          ;; environment just to drop it off the token stream. The FIBRE format
          ;; has its limitations... TODO: alternative output via C?
          (values '#:procedure (cddr tokens)))]
       [_ (error "Incorrect input -- parse closure")])]
    [(7)
     (error "7 is unknown")]
    [(8) ;; Vector
     (match tokens
       [(list-rest '(st) (list num size) '(lbc) _ '(com) _ '(col) rest)
        (let loop ((i size)
                   (vals '())
                   (tokens rest))
          (if (= i 0)
              (values (list->vector (reverse vals)) (cdddr tokens))
              (let-values ([(val tokens) (parse* tokens)])
                (loop (- i 1) (cons val vals) tokens))))]
       [_ (error "Incorrect input -- parse vector")])]))

;; TODO: update reader to make use of parse* everywhere
(define (parse* tokens)
  (match tokens
    [(list-rest '(lpar) (list 'num n) '(col) rest)
     (parse-typedval n rest)]
    [_ (error "Incorrect input -- parse")]))

(define (parse tokens)
  (let-values ([(value ignore) (parse* tokens)])
    value))

#;(define (parse tokens)
(match tokens
  [(list-rest '(lpar) (list 'num n) '(col) rest)
   (let-values ([(value ignore) (parse-typedval n rest)])
     value)]
  [_ (error "Incorrect input -- parse")]))

(define (parse-fibre-input str)
  (parse (scan-fibre str)))

(define (make-thunk lib)
  (let-values ([(s-lib) (ffi-lib lib)]
	       [(in out) (make-cpipe)])
    (let ((ParseCommandLine
           (get-ffi-obj "ParseCommandLine" s-lib
                        (_fun _int _pointer -> _void)))
	  (InitSisalRunTime
           (get-ffi-obj "InitSisalRunTime" s-lib
                        (_fun -> _void)))
	  (ReadFibreInputs
           (get-ffi-obj "ReadFibreInputs" s-lib
                        (_fun -> _pointer)))
	  (StartWorkers
           (get-ffi-obj "StartWorkers" s-lib
                        (_fun -> _void)))
	  (SisalMain
           (get-ffi-obj "SisalMain" s-lib
                        (_fun _pointer -> _void)))
	  (StopWorkers
           (get-ffi-obj "StopWorkers" s-lib
                        (_fun -> _void)))
	  (WriteFibreOutputs
           (get-ffi-obj "WriteFibreOutputs" s-lib
                        (_fun _pointer -> _void)))
	  (ShutDownDsa
           (get-ffi-obj "ShutDownDsa" s-lib
                        (_fun -> _void)))
	  (in-fs (fdopen in "r"))
	  (out-fs (fdopen out "w")))
      (let* ((t (_array _string 2))
             (x (malloc t))
             (a (ptr-ref x t 0)))
        (array-set! a 0 "-gss")
        ;; Set parallelism to 4 workers
        ;; TODO: maybe put it in a config file
        (array-set! a 1 "-w4")

        ;; Let's make this pipe non-blocking
        ;; http://stackoverflow.com/questions/1735781/non-blocking-pipe-using-popen
        ;; Alternative: using select(), but more work
        (let ((d (fileno in-fs)))
          ;; fcntl(d, F_SETFL, O_NONBLOCK);
          (fcntl d F_SETFL O_NONBLOCK))
        (set-ffi-obj! "FibreOutFd" s-lib
                      _file_ptr out-fs)
        (ParseCommandLine 2 x)
        (InitSisalRunTime)
        (set-ffi-obj! "SisalMainArgs" s-lib
                      _pointer (ReadFibreInputs))
        (lambda ()
          (StartWorkers)
          (SisalMain (get-ffi-obj "SisalMainArgs" s-lib _pointer))
          (StopWorkers)
          (WriteFibreOutputs (get-ffi-obj "SisalMainArgs" s-lib _pointer))
          (fflush out-fs)
          ;;(read-fibre (read-from-fs in-fs))
          (let ((str (read-from-fs in-fs)))
            ;;(make-fibre-scanner str))
            (parse-fibre-input str)))))))
