#lang racket

(define (max-operations nums)
  (let iter ([nums nums]
             [pre (+ (first nums) (second nums))]
             [ans 0])
    (match nums
      [(cons a (cons b bs)) (if (= (+ a b) pre) (iter bs pre (add1 ans)) ans)]
      [_ ans])))

(module* test #f
  (require rackunit)
  (check-eq? (max-operations '(3 2 1 4 5)) 2)
  (check-eq? (max-operations '(3 2 6 1 4)) 1))
