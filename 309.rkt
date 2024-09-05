#lang racket

(define (max-profit prices)
  (let loop ([lst prices]
             [pre 0]
             [f0 0]
             [f1 (- (first prices))])
    (match lst
      ['() f0]
      [(cons x xs) (loop xs f0 (max f0 (+ f1 x)) (max f1 (- pre x)))])))

(module+ test
  (require rackunit)
  (check-eq? (max-profit '(1 2 3 0 2)) 3)
  (check-eq? (max-profit '(1)) 0))
