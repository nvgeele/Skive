#!racket

(require ffi/unsafe
	 ffi/unsafe/define
	 "typing.rkt")

(provide make-thunk)

(define-ffi-definer define-cl (ffi-lib "libc"))
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

(define (read-fibre str)
  (let ((input (open-input-string str)))
    (case (read-char input)
      [(#\u28) (let ((id (string->number (string-trim (read-until input #\:)))))
	       (cond 
		 ((= id (- typedval-bool-idx 1)) 'bool)
		 ((= id (- typedval-string-idx 1)) 'string)
		 ((= id (- typedval-float-idx 1)) 'float)
		 ((= id (- typedval-int-idx 1))
		  (string->number (string-trim (read-until input #\u29))))
		 ((= id (- typedval-null-idx 1))
		  null)
		 ((= id (- typedval-cons-idx 1)) 'cons)
		 (else (error "Incorrect FIBRE syntax(2)"))))]
      [else (error "Incorrect FIBRE syntax(1)")])))

(define (scan-number input c)
  '*num*)
;  (let loop ((res (list c))
;	     (c (peek-char input)))
;    (cond ((char-numeric? c)
;	   (loop (cons (read-char input))
;		 (peek-char input)))

(define (make-fibre-scanner str)
  (display str)
  (letrec ((input (open-input-string str))
	   (scanner
	     (lambda (input)
	       (let ((c (read-char input)))
		 (cond ((eof-object? c)
			(values c (lambda () (scanner input))))
		       ((char-whitespace? c)
			(scanner input))
		       ((char=? c #\u28) ;; (
			(values '(lpar) (lambda () (scanner input))))
		       ((char=? c #\u29) ;; )
			(values '(rpar) (lambda () (scanner input))))
		       ((char=? c #\u3c) ;; <
			(values '(st) (lambda () (scanner input))))
		       ((char=? c #\u3e) ;; >
			(values '(gt) (lambda () (scanner input))))
		       ((char=? c #\u3a)
			(values '(col) (lambda () (scanner input))))
		       ((char-numeric? c)
			(values `(num ,(scan-number input c)) (lambda () (scanner input))))
		       ((char=? c #\u23)
			(read-line input) ;; drop all the rubbish
			(scanner input)))))))
    (lambda ()
      (scanner input))))

(define (parse-fibre-input str)
  (let loop ((scanner (make-fibre-scanner str))
	     (res '()))
    (let-values ([(token scanner) (scanner)])
      (if (eof-object? token)
	res
	(begin (display token)(newline)
	       (loop scanner res))))))

(define (make-thunk lib)
  (let-values ([(s-lib) (ffi-lib lib)]
	       [(in out) (make-cpipe)])
    (let ((ParseCommandLine
	    (get-ffi-obj "ParseCommandLine" s-lib
			 (_fun _int (_ptr i _string) -> _void)))
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
      ;; Let's make this pipe non-blocking
      ;; http://stackoverflow.com/questions/1735781/non-blocking-pipe-using-popen
      ;; Alternative: using select(), but more work
      (let ((d (fileno in-fs)))
	;; fcntl(d, F_SETFL, O_NONBLOCK);
	(fcntl d 4 4))
      (set-ffi-obj! "FibreOutFd" s-lib
		    _file_ptr out-fs)
      (ParseCommandLine 0 "null")
      (InitSisalRunTime)
      (set-ffi-obj! "SisalMainArgs" s-lib
		    _pointer (ReadFibreInputs))
      (lambda ()
	(StartWorkers)
	(SisalMain (get-ffi-obj "SisalMainArgs" s-lib _pointer))
	(StopWorkers)
	(WriteFibreOutputs (get-ffi-obj "SisalMainArgs" s-lib _pointer))
	(fflush out-fs)
	;(read-fibre (read-from-fs in-fs))
	(let ((str (read-from-fs in-fs)))
	  ;(make-fibre-scanner str))
	  (parse-fibre-input str))
	))))
