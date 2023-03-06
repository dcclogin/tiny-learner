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

(define logistic-sigmoid
  (λ (x)
    (let ([ex (exp x)])
      (÷ ex (+ 1 ex)))))

(define tanh
  (λ (x)
    (let ([e2x (exp (* 2 x))])
      (÷ (- e2x 1)
         (+ e2x 1)))))

(define leaky-rectify
  (λ (m)
    (λ (x)
      (cond
        [(< x 0) (* m x)]
        [else x]))))

(define softmax
  (λ (x)
    (let ([expd (- (exp x) (max x))])
      (÷ expd (sum expd)))))
