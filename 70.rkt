#lang racket/base

(require racket/match)

(provide climb-stairs)

(define (climb-stairs n)
  (let loop ([n n]
             [a 1]
             [b 2])
    (match n
      [1 a]
      [2 b]
      [_ (loop (- n 1) b (+ a b))])))

(module* test #f
  (require rackunit)
  (check-equal? (climb-stairs 2) 2)
  (check-equal? (climb-stairs 3) 3))
