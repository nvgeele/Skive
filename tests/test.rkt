#lang racket

(require rackunit
         rackunit/text-ui
         "test-expand.rkt"
         "test-native-functions.rkt")

(run-tests
 (test-begin expand-tests
             native-function-tests))
