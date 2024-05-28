#lang racket/base

(provide find-peaks)

(define (find-peaks mountain)
  (let ([len (length mountain)]
        [mountain (list->vector mountain)])
    (let iter ([i 1]
               [res (list)])
      (if (< i (sub1 len))
        (if (and (> (vector-ref mountain i) (vector-ref mountain (sub1 i)))
                 (> (vector-ref mountain i) (vector-ref mountain (add1 i))))
          (iter (+ i 2) (append res (list i)))
          (iter (add1 i) res))
        res))))

(module* test #f
  (require rackunit)
  (check-match (find-peaks '(2 4 4)) '())
  (check-match (find-peaks '(1 4 3 8 5)) '(1 3)))
