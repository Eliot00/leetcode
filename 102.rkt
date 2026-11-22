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

(define/contract (level-order root)
  (-> (or/c tree-node? #f) (listof (listof exact-integer?)))
  (if root
      (let loop ([q (list root)] [ret '()])
           (if (null? q)
               ret
               (loop
                (filter-map identity
                            (flatten (map
                                      (λ (node) (list (tree-node-left node) (tree-node-right node)))
                                      q)))
                (append ret (list (map tree-node-val q))))))
      '()))
