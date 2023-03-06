#lang racket
(require malt)

;; Running down a slippery slope


;; successive approximation : seems to be adopted by multiple fields like psychology

;; rely on `line
#;
(define l2-loss
  (λ (xs ys)
    (λ (θ)
      (let ([ys* ((line xs) θ)])
        (sum (sqr (- ys ys*)))))))

;; `line as a name is too specific!
#;
(define l2-loss
  (λ (line)
    (λ (xs ys)
      (λ (θ)
        (let ([ys* ((line xs) θ)])
          (sum (sqr (- ys ys*))))))))

(define l2-loss
  (λ (target)
    (λ (xs ys)  ;; ⇐ expectant function
      (λ (θ)    ;; ⇐ objective function
        (let ([ys* ((target xs) θ)])
          (sum (sqr (- ys ys*))))))))


(let ([xs (tensor 2.0 1.0 4.0 3.0)]
      [ys (tensor 1.8 1.2 4.2 3.3)]
      [θ (list 0.0 0.0)])
  (((l2-loss line) xs ys) θ))
;; => 33.21

;; let's now increase manually θ₀ a little, say 0.0099
(let ([xs (tensor 2.0 1.0 4.0 3.0)]
      [ys (tensor 1.8 1.2 4.2 3.3)]
      [θ (list 0.0099 0.0)])
  (((l2-loss line) xs ys) θ))
;; => 32.59


;; Questions: should we continue to increase Θ₀? if so, how much?
;; attempt1 : introduce "rate of change" of the objective function.

;; we need a learning rate (α), it's also called step size.
;; The law of revision : θ₀ = θ₀ - α × rate-of-change-of-loss-wrt-θ₀

(let ([α 0.01])
  1)
