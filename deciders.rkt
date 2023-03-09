#lang racket
(require malt)
(require "prelude.rkt")

;; epilogue

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
