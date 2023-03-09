#lang racket
(require malt)
(require "prelude.rkt")

;; epilogue

(define cross-entropy
  (λ (target num-classes)
    (λ (xs ys)
      (λ (θ)
        (let ([ys* ((target xs) θ)])
          (* -1
             (÷ (∙ ys (log ys*))
                num-classes)))))))

(define l1-loss
  (λ (target)
    (λ (xs ys)
      (λ (θ)
        (let ([ys* ((target xs) θ)])
          (sum (abs (- ys ys*))))))))

(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (θ)
        (let ([ys* ((target xs) θ)])
          (sum (sqr (- ys ys*))))))))
