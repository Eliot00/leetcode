#lang racket/base

(provide missing-rolls)

(define (missing-rolls rolls mean n)
  (let* ([total-length (+ n (length rolls))]
         [missing-sum (- (* mean total-length) (apply + rolls))])
    (if (or (< missing-sum n) (> missing-sum (* n 6)))
      '()
      (let* ([quotient (quotient missing-sum n)]
             [remainder (remainder missing-sum n)])
        (append (build-list remainder (lambda (_) (+ quotient 1) ))
                (build-list (- n remainder) (lambda (_) quotient)))))))

(module* test #f
  (require rackunit)
  (check-match (missing-rolls '(3 2 4 3) 4 2) '(6 6))
  (check-match (missing-rolls '(1 5 6) 3 4) '(3 2 2 2))
  (check-match (missing-rolls '(1 2 3 4) 6 4) '())
  (check-match (missing-rolls '(1) 3 1) '(5)))
