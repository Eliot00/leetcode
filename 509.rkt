#lang racket

(define (fib n)
  (let loop ([n n]
             [i 0]
             [j 1])
    (match n
      [0 i]
      [1 j]
      [_ (loop (sub1 n) j (+ i j))])))

(module* test #f
  (require rackunit)
  (check-eq? (fib 2) 1)
  (check-eq? (fib 3) 2)
  (check-eq? (fib 4) 3))
