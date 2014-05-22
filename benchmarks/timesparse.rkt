#lang racket

(require math/statistics)

(define (get-seconds str)
  (string->number (last (regexp-match #rx".*real 0m([0-9.]+)s.*" str))))

(define (parse in)
  (let outer-loop ((results '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
          results
          (let inner-loop ((threads (string->number line))
                           (times '()))
            (let ((line (read-line in)))
              (if (or (eof-object? line)
                      (string=? line ""))
                  (outer-loop (cons (list threads times) results))
                  (inner-loop threads
                              (cons (get-seconds line) times)))))))))

(define (go)
  (call-with-input-file "/tmp/times" parse))

(define (analysis data)
  (for/list ([benchmark data])
    (let ((times (cadr benchmark)))
      (display "Threads: ")(display (car benchmark))(newline)
      (display "Mean: ")(display (mean times))(newline)
      (newline))))

(define (interleave lst x)
  (let loop ((lst (reverse lst))
             (res '()))
    (cond ((null? lst) res)
          ((null? (cdr lst)) (cons (car lst) res))
          (else (loop (cdr lst)
                      (cons x (cons (car lst) res)))))))

(define (times->mathematica times)
  (let loop ((times (sort times (lambda (a b) (> (car a) (car b)))))
             (strs '()))
    (if (null? times)
        (~a "{" (apply ~a (interleave strs ",\n")) "}")
        (loop (cdr times)
              (cons (~a "{"
                        (apply ~a (interleave (cadar times) ", "))
                        "}")
                    strs)))))

(define (times->csv-files times prefix)
  (call-with-output-file (build-path prefix "config.cfg")
    (lambda (cfgout)
      (display "Name,Sample1,Sample2,ConfLevel,Coef\n" cfgout)
      (let loop ((times (sort times (lambda (a b) (> (car a) (car b))))))
        (unless (null? times)
          (display (~a (caar times) ","
                       "./1.dat,"
                       "./" (caar times) ".dat,NA,NA\n")
                   cfgout)
          (call-with-output-file (build-path prefix (~a (caar times) ".dat"))
            (lambda (out)
              (for ([time (cadar times)])
                (display time out)
                (newline out))))
          (loop (cdr times)))))))
