(define (num-rescue-boats people limit)
  (let ([people (list->vector (sort people <))])
    (let iter ([light 0]
               [heavy (sub1 (vector-length people))]
               [ans 0])
      (if (<= light heavy)
        (if (> (+ (vector-ref people light) (vector-ref people heavy)) limit)
          (iter light (sub1 heavy) (add1 ans))
          (iter (add1 light) (sub1 heavy) (add1 ans)))
        ans))))
