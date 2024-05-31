#lang racket

(provide find-missing-and-repeated-values)

(define (find-missing-and-repeated-values grid)
  (let* ([m (expt (length grid) 2)]
         [flat-grid (flatten grid)]
         [d1 (- (for/sum ([i (in-list flat-grid)]) i)
                (quotient (* m (add1 m)) 2))]
         [d2 (- (for/sum ([i (in-list flat-grid)]) (* i i))
                (quotient (* m (add1 m) (add1 (* m 2))) 6))])
    (list (quotient (+ (quotient d2 d1) d1) 2)
          (quotient (- (quotient d2 d1) d1) 2))))

(module* test #f
  (require rackunit)
  (check-match (find-missing-and-repeated-values
                  (list '(1 3) '(2 2))) '(2 4))
  (check-match (find-missing-and-repeated-values
                  (list '(9 1 7) '(8 9 2) '(3 4 6))) '(9 5)))

