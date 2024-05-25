#lang racket/base

(provide find-indices)

(define (find-indices nums index-difference value-difference)
  (define (helper min-index max-index j)
    (if (>= j (length nums))
        (list -1 -1)
        (let* ([i (- j index-difference)]
               [min-index (if (< (list-ref nums i) (list-ref nums min-index)) i min-index)]
               [max-index (if (> (list-ref nums i) (list-ref nums max-index)) i max-index)])
          (cond
            [(>= (- (list-ref nums j) (list-ref nums min-index)) value-difference) (list min-index j)]
            [(>= (- (list-ref nums max-index) (list-ref nums j)) value-difference) (list max-index j)]
            [else (helper min-index max-index (+ j 1))]))))
  (helper 0 0 index-difference))

(module* test #f
  (require rackunit)
  (check-match (find-indices '(5 1 4 1) 2 4) '(0 3))
  (check-match (find-indices '(2 1) 0 0) '(0 0))
  (check-match (find-indices '(1 2 3) 2 4) '(-1 -1)))
