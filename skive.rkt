#!/usr/bin/env racket
#lang racket

(require racket/cmdline
         "src/compile.rkt")

(define output-file (make-parameter #f))
(define if1-mode (make-parameter #f))

(define input-file
  (command-line
   #:program "skive"
   #:once-each
   [("--IF1") "Do not compile, only produce IF1 code" (if1-mode #t)]
   [("-o" "--out") file "Output file" (output-file file)]
   #:args (filename)
   filename))

(define (read-all input)
  (let loop ((res '())
             (cur (read input)))
    (if (eof-object? cur)
        res
        (loop (append res `(,cur))
              (read input)))))

;; TODO: use file->list instead of read-all
(define (compile input output-file)
  (let* ((skive-code (last (read-all input)))
         (if1-code (compile-sequence-to-if1 skive-code)))
    (if (if1-mode)
        (call-with-output-file (if output-file output-file "if1.out")
          (lambda (out)
            (display if1-code out)))
        (compile-if1-to-native if1-code
                               #:path (if output-file output-file "s.out")
                               #:type 'exe))
    (void)))

(call-with-input-file
    input-file
  (lambda (input) (compile input (output-file))))
