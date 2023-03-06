#lang racket
(require malt)
(require "../prelude.rkt")
(require "../data.rkt")

;; Target practice

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

(define quad
  (λ (t)
    (λ (θ)
      (+ (* (ref θ 0) (sqr t))
         (+ (* (ref θ 1) t)
            (ref θ 2))))))

((quad 3.0) (list 4.5 2.1 7.8))


(with-hypers
  ([α 0.001]
   [revs 1000])
  (gradient-descent
   ((l2-loss quad) quad-xs quad-ys)
   (list 0.0 0.0 0.0)))


;; ------------ let go off ------------- ;;


(define plane
  (λ (t)
    (λ (θ)
      (+ (∙ (ref θ 0) t)
         (ref θ 1)))))


(with-hypers
  ([α 0.001]
   [revs 1000])
  (gradient-descent
   ((l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))


;; The rule of parameters (final ver.) : every parameter is a tensor.
;; The rule of θ : θ is a list of parameters that can have different shapes.
