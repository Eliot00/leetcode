(define (test solution case-list)
  (map (lambda (case)
         (let* ((params-list (car case))
                (expect (cdr case))
                (actual (apply solution params-list)))
           (list params-list expect actual)))
       case-list))

(provide 'test)
