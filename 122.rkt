#lang racket

(define (max-profit prices)
  (let loop ([lst prices]
             [dp0 0]
             [dp1 (- (first prices))])
    (match lst
      ['() dp0]
      [(cons x xs) (loop xs (max dp0 (+ dp1 x)) (max dp1 (- dp0 x)))])))

(module+ test
  (require rackunit)
  (check-eq? (max-profit '(7 1 5 3 6 4)) 7)
  (check-eq? (max-profit '(1 2 3 4 5)) 4)
  (check-eq? (max-profit '(7 6 4 3 1)) 0))
