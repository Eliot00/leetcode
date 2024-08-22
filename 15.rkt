#lang racket

(define (three-sum nums)
  (let* ([len (length nums)]
         [vec (list->vector nums)]
         ; 和=0，那么排序后头尾必须一正一负，双指针移动也方便
         [sorted (vector-sort vec <)])
    (if (< len 3)
      0
      ; 三维flat到二维
      (filter (λ (v) (eq? 3 (length v)))
        (apply append
          (for/list ([i (in-range len)]
                     ; 排序后base > 0，后面不可能相加=0了，break
                     #:break (> (vector-ref sorted i) 0)
                     ; 跳过相等的base
                     #:when (or (= i 0)
                                (< (vector-ref sorted (sub1 i))
                                   (vector-ref sorted i))))
            (move sorted i (add1 i) (sub1 len) (list '()))))))))

; 自定义move函数，麻烦的是找到了答案时不能直接返回，双指针之间可能还有解
(define (move vec base left right res)
  (if (>= left right)
    res
    (let ([sum (my-sum vec base left right)])
      (cond
        [(> sum 0) (move vec base left (sub1 right) res)]
        [(< sum 0) (move vec base (add1 left) right res)]
        ;这里racket的list操作可能会很耗时
        [else (move
                vec
                base
                (move-until-not-eq vec left (λ (x) (= x right)) (vector-ref vec left) add1)
                (move-until-not-eq vec right (λ (x) (= x left)) (vector-ref vec right) sub1)
                (append res (list (list (vector-ref vec base)
                                        (vector-ref vec left)
                                        (vector-ref vec right)))))]))))

; 确保移动指针到不同的值上
(define (move-until-not-eq vec pointer border? last-v move-action)
  ; 到达边界直接返回
  (if (border? pointer)
    pointer
    (if (= (vector-ref vec pointer) last-v)
      ; 如果等于上一个值用move-action函数更新指针
      (move-until-not-eq vec (move-action pointer) border? last-v move-action)
      pointer)))

(define (my-sum vec p1 p2 p3)
  (+ (vector-ref vec p1)
     (vector-ref vec p2)
     (vector-ref vec p3)))

