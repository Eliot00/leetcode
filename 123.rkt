#lang racket

(define (max-profit prices)
  (let loop ([lst prices]
             [buy1 -100000]
             [sell1 0]
             [buy2 -100000]
             [sell2 0])
    (match lst
      ['() sell2]
      [(cons x xs) (loop
                     xs
                     (max buy1 (- x))
                     (max sell1 (+ buy1 x))
                     (max buy2 (- sell1 x))
                     (max sell2 (+ buy2 x)))])))

; 根据前提，能买说明一定是没操作过或买卖过一次
; 能卖一定是买过一次或买卖过一次又买了一次
; buy1 = max(buy1,  -p(i)) 之前买过一次了，或今天买
; sell1 = max(sell1, buy1 + p(i)) 之前买了也卖过一次了，或之前买了今天卖
; buy2 = max(buy2, buy1 - p(i)) 之前买过两次，或买卖过一次，今天再买一次
; sell2 = max(sell2, buy2 + p(i)) 之前做完了两次交易，或买两卖一、今天卖最后一次
