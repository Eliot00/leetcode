; Definition for a binary tree node.
#|

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

|#

(define/contract (zigzag-level-order root)
  (-> (or/c tree-node? #f) (listof (listof exact-integer?)))
  (if (not root)
      '()
      (let ([q (make-queue)])
        (enqueue! q root)
        (let loop ([ans '()] [ltr? #t])
          (if (queue-empty? q)
              (reverse ans)
              (let ([level-size (queue-length q)])
                (let level-loop ([i 0] [cur '()])
                  (if (= i level-size)
                      ;; 本层结束：根据方向决定是否反转，然后翻转向标志
                      (loop (cons (if ltr? (reverse cur) cur) ans)
                            (not ltr?))
                      (let ([node (dequeue! q)])
                        ;; 先左后右入队，保持队列的层序
                        (when (tree-node-left node)
                          (enqueue! q (tree-node-left node)))
                        (when (tree-node-right node)
                          (enqueue! q (tree-node-right node)))
                        ;; cons 前置累积（得到反序）
                        (level-loop (+ i 1)
                                    (cons (tree-node-val node) cur)))))))))))