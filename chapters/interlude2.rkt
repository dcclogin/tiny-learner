#lang racket
(require malt)

;; Too many toys make us hyperactive

(declare-hyper α)
(declare-hyper revs)

(define gradient-descent
  (λ (obj θ)
    (let ([f (λ (Θ)
               (let ([gs (∇ obj Θ)])
                 (map (λ (p q) (- p (* α q)))
                      Θ
                      gs)))])
      (revise f revs θ))))

(with-hypers ([α 0.01]
              [revs 1000])  ;; ⇐ dynamic scoping
  (let ([xs (tensor 2.0 1.0 4.0 3.0)]
        [ys (tensor 1.8 1.2 4.2 3.3)])
    (gradient-descent
     ((l2-loss line) xs ys)
     (list 0.0 0.0))))
