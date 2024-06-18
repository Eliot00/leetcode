#lang racket

(define (discount-prices sentence discount)
  (define (process-word word)
    (if (is-price-string? word)
      (let* ([price (string->number (substring word 1))]
             [discounted-price (* price (/ (- 100 discount) 100))])
        (string-append "$" (~r discounted-price #:precision '(= 2))))
      word))
  (string-join (map process-word (string-split sentence " "))))

(module+ test
  (require rackunit)
  (check-equal? (discount-prices "there are $1 $2 and 5$ candies in the shop" 50)
             "there are $0.50 $1.00 and 5$ candies in the shop")
  (check-equal? (discount-prices "1 2 $3 4 $5 $6 7 8$ $9 $10$" 100)
             "1 2 $0.00 4 $0.00 $0.00 7 8$ $0.00 $10$"))

(define (is-price-string? str)
  (regexp-match? #px"^\\$\\d+$" str))

(module+ test
  (require rackunit)
  (check-true (is-price-string? "$100"))
  (check-true (is-price-string? "$23"))
  (check-false (is-price-string? "100"))
  (check-false (is-price-string? "$1e5"))
  (check-false (is-price-string? "1$0")))
