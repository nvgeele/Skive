#lang racket

(provide
 ;; COMPILER CONFIGURATION
 sisal-compiler-path
 gcc-path
 runtime-object-path
 sisal-lib-path
 sisal-include-path
 ;; OPTIONS FOR FFI
 O_NONBLOCK
 F_SETFL
 CLIB
 num-workers)

(define sisal-compiler-path
  "/usr/local/bin/sisalc")
(define gcc-path
  "/usr/bin/gcc")
(define runtime-object-path
  "/usr/local/lib/sisal/srt0.o")
(define sisal-lib-path
  "/usr/local/lib/sisal")
(define sisal-include-path
  "/usr/local/include/sisal")

;; The following options may depend on other factors than just the LINUX kernel
(define O_NONBLOCK
  2048)
(define F_SETFL
  4)
(define CLIB
  #f)
(define num-workers
  4)
