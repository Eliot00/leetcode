#lang racket

(define (change amount coins)
  (let* ([n (length coins)]
         [coins (list->vector coins)]
         [dp (build-vector (add1 amount) (lambda (a) 0))])
    (vector-set! dp 0 1)
    (for ([i (in-range 1 (add1 n))])
      (for ([a (in-range 1 (add1 amount))])
        (if (> (vector-ref coins (sub1 i)) a)
          void
          (vector-set! dp a (+ (vector-ref dp a) (vector-ref dp (- a (vector-ref coins (sub1 i)))))))))
    (vector-ref dp amount)))
