#lang racket

(define (find-lu-slength strs)
  (for/fold ([ans -1])
            ([i (in-naturals)]
             [s (in-list strs)])
    (let ([check (for/and ([j (in-naturals)]
                          [t (in-list strs)])
                   (or (= i j) (not (is-sub s t))))])
      (if check
        (max ans (string-length s))
        ans))))

(module+ test
  (require rackunit)
  (check-eq? (find-lu-slength '("aba" "cdc" "eae")) 3)
  (check-eq? (find-lu-slength '("aaa" "aaa" "aa")) -1))

(define (is-sub str-a str-b)
  (let ([len-a (string-length str-a)]
       [len-b (string-length str-b)])
    (let iter ([p-a 0]
               [p-b 0])
      (if (and (< p-a len-a) (< p-b len-b))
        (iter (if (char=? (string-ref str-a p-a) (string-ref str-b p-b)) (add1 p-a) p-a) (add1 p-b))
        (= p-a len-a)))))

(module+ test
  (require rackunit)
  (check-true (is-sub "aa" "aaa"))
  (check-false (is-sub "aba" "cdc")))
