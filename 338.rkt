#lang racket

(define (count-bits n)
  (let* ([len (add1 n)]
         [ans (make-vector len 0)])
    (for ([i (in-range len)])
      (if (even? i)
        (vector-set! ans i (vector-ref ans (quotient i 2)))
        (vector-set! ans i (add1 (vector-ref ans (quotient i 2))))))
    (vector->list ans)))

(module* test #f
  (require rackunit)
  (check-match (count-bits 2) '(0 1 1))
  (check-match (count-bits 5) '(0 1 1 2 1 2)))
