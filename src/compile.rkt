#!racket

(require "ffi.rkt"
	 "expand.rkt"
	 "analyse.rkt"
	 "generate.rkt"
	 "translate.rkt")

(provide compile-sequence-to-if1
         compile-skive-to-if1
	 compile-if1-to-native)

;; TODO: make config file for this
(define sisal-compiler-path "/usr/local/bin/sisalc")
(define gcc-path "/usr/bin/gcc")
(define runtime-object-path "/usr/local/lib/sisal/srt0.o")
(define sisal-lib-path "/usr/local/lib/sisal")
(define sisal-include-path "/usr/local/include/sisal")
(define graphviz-dot-path "/usr/local/bin/dot")

(define (compile-sequence-to-if1 sequence)
  (compile-skive-to-if1 `(begin ,@sequence)))

;; TODO: add output path variable
(define (compile-skive-to-if1 expression)
  (let* ((expanded (expand expression))
	 (analysed (analyse expanded))
	 (generated (generate analysed))
	 (translated (translate generated)))
    translated))

;; TODO: add output path variable
(define (compile-if1-to-native code #:type [type 'exe])
  (let* ((code-prefix (path->string (make-temporary-file "skiveif1~a" #f "/tmp")))
	 (code-file (string-append code-prefix ".if1"))
	 (csrc-file (string-append code-prefix ".c"))
	 (cobj-file (string-append code-prefix ".o"))
	 (out-file (string-append code-prefix
				  (case type
				    [(exe) ""]
				    [(lib) ".dylib"]
				    [else (error "Incorrect output type -- compile-native")])))
	 (out (open-output-file code-file #:exists 'truncate)))
    (display code out)
    (close-output-port out)
    (let-values ([(sp out in err) (subprocess #f #f #f
					      sisal-compiler-path
					      "-C" code-file)])
      (subprocess-wait sp)
      (close-output-port in)(close-input-port out)(close-input-port err)
      (if (file-exists? csrc-file)
          (let-values ([(sp out in err) (subprocess #f #f #f
                                                    gcc-path csrc-file
                                                    "-fPIC"
                                                    "-c" "-o" cobj-file
                                                    (~a "-I" sisal-include-path)
                                                    "-g" "-O2")])
            (subprocess-wait sp)
            (close-output-port in)(close-input-port out)(close-input-port err)
            (if (file-exists? cobj-file)
                (let-values ([(sp out in err) (subprocess #f #f #f
                                                          gcc-path
                                                          "-o" out-file
                                                          runtime-object-path
                                                          cobj-file
                                                          (if (eq? type 'lib)
                                                              "-shared" "")
                                                          (~a "-L" sisal-lib-path)
                                                          "-lsisal" "-lm")])
                  (subprocess-wait sp)
                  (close-output-port in)(close-input-port out)(close-input-port err)
                  (delete-file code-file)(delete-file csrc-file)(delete-file cobj-file)
                  (unless (eq? type 'exe) (delete-file code-prefix))
                  (if (file-exists? out-file)
                      out-file
                      (error "Could not compile output file -- compile-if1-to-native")))
                (error "Could not compile C source to object file -- compile-if1-to-native")))
          (error "Could not create C source file -- compile-if1-to-native")))))
