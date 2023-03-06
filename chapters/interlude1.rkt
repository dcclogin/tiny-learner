#lang racket
(require malt)

;; The more we extend, the less tensor we get...

(define sum¹
  (λ (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (λ (t i a)
    (cond
      [(zero? i) (+ (tref t 0) a)]
      [else (summed t (sub1 i) (+ (tref t i) a))])))


(sum¹ (tensor 10 12 14))
(sum¹ (tensor (tensor 1 2)
              (tensor 3 4)))
(sum¹ (tensor (tensor (tensor 1 2)
                      (tensor 3 4))
              (tensor (tensor 5 6)
                      (tensor 7 8))))

(sum (tensor (tensor (tensor 1 2)
                     (tensor 3 4))
             (tensor (tensor 5 6)
                     (tensor 7 8))))

((line (tensor 2 7 5 11)) (list 4 6))
