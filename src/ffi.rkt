#!racket

(require ffi/unsafe
	 ffi/unsafe/define)

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
	(read-from-fs in-fs)))))
