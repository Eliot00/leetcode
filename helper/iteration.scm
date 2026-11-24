(define-module (helper iteration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-41)
  #:export (in-range
            in-list
            in-vector
            in-string
            in-naturals
            in-indexed
            for/fold
            for*/fold
            for
            for*
            for/sum
            for/max
            for/min
            for/count
            for*/count
            for/and
            for/or))

(define in-range
  (case-lambda
    [(end) (in-range 0 end 1)]
    [(start end) (in-range start end 1)]
    [(start end step)
     (when (zero? step)
       (error "in-range: step must be non-zero" step))
     (let loop ([i start])
       (if ((if (positive? step) >= <=) i end)
           stream-null
           (stream-cons i (loop (+ i step)))))]))

(define (in-list lst)
  (list->stream lst))

(define in-vector
  (case-lambda
    [(v) (list->stream (vector->list v))]
    [(v start)
     (list->stream (drop (vector->list v) start))]
    [(v start end)
     (list->stream (take (drop (vector->list v) start) (- end start)))]))

(define in-string
  (case-lambda
    [(s) (list->stream (string->list s))]
    [(s start) (list->stream (string->list s start))]
    [(s start end) (list->stream (string->list s start end))]))

(define in-naturals
  (case-lambda
    [() (in-naturals 0)]
    [(start) (let loop ([i start])
               (stream-cons i (loop (+ i 1))))]))

(define (in-indexed strm)
  (let loop ([i 0] [s strm])
    (if (stream-null? s)
        stream-null
        (stream-cons (cons i (stream-car s))
                     (loop (+ i 1) (stream-cdr s))))))

(define (fold-streams accs streams proc)
  (let loop ([accs accs] [streams streams])
    (if (or (null? streams) (any stream-null? streams))
        (apply values accs)
        (loop (proc (map stream-car streams) accs)
              (map stream-cdr streams)))))

(define-syntax for/fold
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([acc init] ...) ([x seq] ...) body ...)
       #'(fold-streams (list init ...)
                       (list seq ...)
                       (lambda (xs accs)
                         (let-values ([(x ...) (apply values xs)]
                                      [(acc ...) (apply values accs)])
                           (call-with-values (lambda () body ...) list))))])))

(define-syntax for*/fold
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([acc init] ...) () body ...)
       #'(let ([acc init] ...) body ...)]
      [(_ ([acc init] ...) ([x seq] rest ...) body ...)
       #'(for/fold ([acc init] ...) ([x seq])
                   (for*/fold ([acc acc] ...) (rest ...) body ...))])))

(define-syntax for
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(begin
           (for/fold ([ignored #f]) ([x seq] ...)
             (begin body ... #f))
           (if #f #f))])))

(define-syntax for*
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(begin
           (for*/fold ([ignored #f]) ([x seq] ...)
             (begin body ... #f))
           (if #f #f))])))

(define-syntax for/sum
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(for/fold ([total 0]) ([x seq] ...)
           (+ total (let () body ...)))])))

(define-syntax for/max
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(for/fold ([best #f]) ([x seq] ...)
           (let ([v (let () body ...)])
             (if best (max best v) v)))])))

(define-syntax for/min
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(for/fold ([best #f]) ([x seq] ...)
           (let ([v (let () body ...)])
             (if best (min best v) v)))])))

(define-syntax for/count
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(for/fold ([n 0]) ([x seq] ...)
           (if (let () body ...) (+ n 1) n))])))

(define-syntax for*/count
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(for*/fold ([n 0]) ([x seq] ...)
           (if (let () body ...) (+ n 1) n))])))

(define-syntax for/and
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(call/cc
          (lambda (break)
            (for/fold ([ans #t]) ([x seq] ...)
              (if (let () body ...) ans (break #f)))))])))

(define-syntax for/or
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([x seq] ...) body ...)
       #'(call/cc
          (lambda (break)
            (for/fold ([ans #f]) ([x seq] ...)
              (if (let () body ...) (break #t) ans))))])))
