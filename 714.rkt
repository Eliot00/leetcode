#lang racket

(define (max-profit prices fee)
  (let loop ([lst prices]
             [dp0 0]
             [dp1 (- 0 (first prices) fee)])
    (match lst
      ['() dp0]
      [(cons x xs) (loop xs (max dp0 (- (+ dp1 x) fee)) (max dp1 (- dp0 x)))])))

(module+ test
  (require rackunit)
  (check-eq? (max-profit '(1 3 2 8 4 9) 2) 8)
  (check-eq? (max-profit '(1 3 7 5 10 3) 3) 6))
