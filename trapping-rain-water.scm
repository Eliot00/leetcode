(use-modules (srfi srfi-64))

(define (trap-monotonic-stack height)
  (let loop ((height height)
             (i 0)
             (stack '())
             (ans 0))
    (if (null? height)
        ans
        (let ((h (car height)))
          (cond ((null? stack)
                 (loop (cdr height) (1+ i) (cons (cons i h) stack) ans))
                ((< h (cdr (car stack)))
                 (loop (cdr height) (1+ i) (cons (cons i h) stack) ans))
                (else
                 (let ((bottom-h (cdr (car stack)))
                       (pop-stack (cdr stack)))
                   (if (null? pop-stack)
                       (loop (cdr height) (1+ i) (cons (cons i h) pop-stack) ans)
                       (let* ((left (car pop-stack))
                              (left-i (car left))
                              (left-h (cdr left))
                              (width (- i left-i 1))
                              (height-diff (- (min left-h h) bottom-h)))
                         (loop height
                               i
                               pop-stack
                               (+ ans (* width height-diff))))))))))))

(test-begin "trap-monotonic-stack-tests")

(test-eqv "case 1" 6 (trap-monotonic-stack '(0 1 0 2 1 0 1 3 2 1 2 1)))
(test-eqv "case 2" 9 (trap-monotonic-stack '(4 2 0 3 2 5)))

(test-end "trap-monotonic-stack-tests")

(define (prefix-max lst)
  (let loop ((lst lst)
             (max-val -1)
             (result '()))
    (if (null? lst)
        (reverse result)
        (let ((new-max (max max-val (car lst))))
          (loop (cdr lst) new-max (cons new-max result))))))

(define (suffix-max lst)
  (let loop ((lst (reverse lst))
             (max-val -1)
             (result '()))
    (if (null? lst)
        result
        (let ((new-max (max max-val (car lst))))
          (loop (cdr lst) new-max (cons new-max result))))))

(define (trap-pre-suf height)
  (let loop ((pre-max (prefix-max height))
             (suf-max (suffix-max height))
             (height height)
             (ans 0))
    (if (null? height)
        ans
        (let ((p (car pre-max))
              (s (car suf-max))
              (h (car height)))
          (loop (cdr pre-max)
                (cdr suf-max)
                (cdr height)
                (+ ans (- (min p s) h)))))))

(test-begin "trap-pre-suf")

(test-eqv "case 1" 6 (trap-pre-suf '(0 1 0 2 1 0 1 3 2 1 2 1)))
(test-eqv "case 2" 9 (trap-pre-suf '(4 2 0 3 2 5)))

(test-end "trap-pre-suf")

(define (trap-two-pointers height)
  (let ((n (vector-length height)))
    (let loop ((left 0)
               (right (- n 1))
               (pre-max 0)
               (suf-max 0)
               (ans 0))
      (if (< left right)
          (let ((new-pre-max (max pre-max (vector-ref height left)))
                (new-suf-max (max suf-max (vector-ref height right))))
            (if (< new-pre-max new-suf-max)
                (loop (+ left 1)
                      right
                      new-pre-max
                      new-suf-max
                      (+ ans (- new-pre-max (vector-ref height left))))
                (loop left
                      (- right 1)
                      new-pre-max
                      new-suf-max
                      (+ ans (- new-suf-max (vector-ref height right))))))
          ans))))

(test-begin "trap-two-pointers")

(test-eqv "case 1" 6 (trap-two-pointers #(0 1 0 2 1 0 1 3 2 1 2 1)))
(test-eqv "case 2" 9 (trap-two-pointers #(4 2 0 3 2 5)))

(test-end "trap-two-pointers")
