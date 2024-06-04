#lang racket

(define (unique-paths-with-obstacles obstacleGrid)
  (let ([grid (list->vector (map list->vector obstacleGrid))])
    (if (= (vector-ref (vector-ref grid 0) 0) 1)
      0
      (let* ([m (vector-length grid)]
             [n (vector-length (vector-ref grid 0))]
             [dp (make-vector (add1 n) 0)])
        (vector-set! dp 1 1)
        (for ([i (in-range m)])
          (for ([j (in-range n)])
            (if (= (vector-ref (vector-ref grid i) j) 1)
              (vector-set! dp (add1 j) 0)
              (vector-set! dp (add1 j) (+ (vector-ref dp (add1 j)) (vector-ref dp j))))))
        (vector-ref dp n)))))

(module* test #f
  (require rackunit)
  (check-eq?
    (unique-paths-with-obstacles (list '(0 0 0) '(0 1 0) '(0 0 0)))
    2)
  (check-eq?
    (unique-paths-with-obstacles (list '(0 1) '(0 0)))
    1))
