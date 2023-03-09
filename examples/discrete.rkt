#lang racket
(require malt)

;; set target function for θ₀ + x = y
(define single
  (λ (x)
    (λ (θ)
      (+ x (ref θ 0)))))

;; just one point!
(define single-x [tensor 2])
(define single-y [tensor 5])



(declare-hyper revs)
(declare-hyper α)

(define gradient-descent
  (λ (obj θ)
    (let ([f (λ (Θ)
               (let ([gs (∇ obj Θ)])
                 (map (λ (p q) (- p (* α q)))
                      Θ
                      gs)))])
      (revise f revs θ))))


;; it can learn θ to be 3.0 with the following setting:
(with-hypers
  ([α 0.5]
   [revs 1])
  (gradient-descent
   ((l2-loss single) single-x single-y)
   (list 0.0)))


;; learn θ to be 2.9999999993888893
(with-hypers
  ([α 0.1]
   [revs 100])
  (gradient-descent
   ((l2-loss single) single-x single-y)
   (list 0.0)))
