#lang racket

(provide distribute-candies)

(define (distribute-candies candies num_people)
  (let* ([n num_people]
         [p (inexact->exact (floor (- (sqrt (+ (* 2.0 candies) 0.25)) 0.5)))]
         [remaining (- candies (quotient (* (add1 p) p) 2))]
         [rows (quotient p n)]
         [cols (modulo p n)])
    (for/list ([i (in-range n)])
      (let ([base (+ (* (add1 i) rows) (* (quotient (* rows (sub1 rows)) 2) n))])
        (if (< i cols)
          (+ base (+ i 1 (* rows n)))
          (if (= i cols)
            (+ base remaining)
            base))))))

(module* test #f
  (require rackunit)
  (check-match (distribute-candies 7 4) '(1 2 3 1))
  (check-match (distribute-candies 10 3) '(5 2 3)))
