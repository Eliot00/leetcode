#lang racket

(define (count-battleships board)
  (let ([grid (list->vector (map list->vector board))])
    (for*/sum ([i (in-range (length board))]
               [j (in-range (length (first board)))])
      (if (and (char=? (vector-ref (vector-ref grid i) j) #\X)
               (or (= i 0) (not (char=? (vector-ref (vector-ref grid (sub1 i)) j) #\X)))
               (or (= j 0) (not (char=? (vector-ref (vector-ref grid i) (sub1 j)) #\X))))
        1
        0))))

(module* test #f
  (require rackunit)
  (check-eq? (count-battleships (list '(#\.))) 0)
  (check-eq? (count-battleships (list '(#\X #\. #\. #\X)
                                      '(#\. #\. #\. #\X)
                                      '(#\. #\. #\. #\X))) 2))
