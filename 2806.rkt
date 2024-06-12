#lang racket

(define (account-balance-after-purchase purchaseAmount)
  (- 100 (* 10 (quotient (+ purchaseAmount 5) 10))))

(module+ test
  (require rackunit)
  (check-eq? (account-balance-after-purchase 9) 90)
  (check-eq? (account-balance-after-purchase 15) 80))
