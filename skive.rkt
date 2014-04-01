#lang racket

(require racket/cmdline
         "src/compile.rkt")

(define output-file (make-parameter #f))
(define input-file
  (command-line
   #:program "skive"
   #:once-each
   [("-o" "--out") file "output file" (output-file file)]
   #:args (filename)
   filename))

(define (read-all input)
  (let loop ((res '())
             (cur (read input)))
    (if (eof-object? cur)
        res
        (loop (append res `(,cur))
              (read input)))))

(define (compile input output-file)
  (let* ((skive-code (read-all input))
         (if1-code (compile-sequence-to-if1 skive-code)))
    (compile-if1-to-native if1-code
                           #:path (if output-file output-file "s.out")
                           #:type 'exe)
    (void)))

(call-with-input-file
    input-file
  (lambda (input) (compile input (output-file))))
