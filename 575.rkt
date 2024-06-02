#lang racket

(provide distribute-candies)

(define (distribute-candies candyType)
  (let ([limit (quotient (length candyType) 2)])
    (let iter ([lst candyType]
               [type-count (make-immutable-hash)])
      (match lst
        ['() (min limit (hash-count type-count))]
        [(cons x xs) (iter xs (hash-update type-count x add1 0))]))))

(module* test #f
  (require rackunit)
  (check-eq? (distribute-candies '(1 1 2 2 3 3)) 3)
  (check-eq? (distribute-candies '(1 1 2 3)) 2)
  (check-eq? (distribute-candies '(6 6 6 6)) 1))
