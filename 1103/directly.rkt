#lang racket

(provide distribute-candies)

(define (distribute-candies candies num_people)
  (let iter ([i 0]
             [rst candies]
             [ans (make-vector num_people 0)])
    (if (= rst 0)
      (vector->list ans)
      (let ([new-i (add1 i)]
            [idx (modulo i num_people)])
        (iter
          new-i
          (- rst (min new-i rst))
          (vector-set/copy
            ans
            idx
            (+ (vector-ref ans idx) (min new-i rst))))))))

(module* test #f
  (require rackunit)
  (check-match (distribute-candies 7 4) '(1 2 3 1))
  (check-match (distribute-candies 10 3) '(5 2 3)))
