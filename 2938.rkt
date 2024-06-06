#lang racket

(define (minimum-steps s)
  (for/fold ([ans 0]
             [cnt1 0]
             #:result ans)
            ([c (in-string s)])
    (if (char=? c #\1)
      (values ans (add1 cnt1))
      (values (+ ans cnt1) cnt1))))

(module* test #f
  (require rackunit)
  (check-eq? (minimum-steps "101") 1)
  (check-eq? (minimum-steps "100") 2)
  (check-eq? (minimum-steps "0111") 0))
