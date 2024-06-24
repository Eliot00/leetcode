#lang racket

(define (detect-capital-use word)
  (let ([cnt (for/sum ([c (in-string word)]) (if (char-upper-case? c) 1 0))])
    (or (= cnt 0)
        (= cnt (string-length word))
        (and (= cnt 1)
             (char-upper-case? (string-ref word 0))))))

(module+ test
  (require rackunit)
  (check-true (detect-capital-use "USA"))
  (check-false (detect-capital-use "FlaG")))
