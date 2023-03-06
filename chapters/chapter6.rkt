#lang racket
(require malt)
(require "../data.rkt")

;; An Apple a Day

;; sampling comes into the picture


;; The rule of batches : A batch of indices consists of random indices which are natural numbers smaller than len(xs)

(define samples
  (λ (n s)
    (sampled n s (list))))

(define sampled
  (λ (n i a)
    (cond
      [(zero? i) a]
      [else (sampled n (sub1 i)
                     (cons (random n) a))])))

(declare-hyper batch-size)
(declare-hyper α)
(declare-hyper revs)

(define sampling-obj
  (λ (expectant xs ys)
    (let ([n (tlen xs)])
      (λ (θ)
        (let ([b (samples n batch-size)])
          ((expectant (trefs xs b) (trefs ys b)) θ))))))

(define gradient-descent
  (λ (obj θ)
    (let ([f (λ (Θ)
               (let ([gs (∇ obj Θ)])
                 (map (λ (p q) (- p (* α q)))
                      Θ
                      gs)))])
      (revise f revs θ))))

(with-hypers
  ([revs 1000]
   [α 0.01]
   [batch-size 2])
  (gradient-descent
   (sampling-obj (l2-loss line) line-xs line-ys)
   (list 0.0 0.0)))


;; stochastic gradient descent


(with-hypers
  ([revs 15000]
   [α 0.001]
   [batch-size 4])
  (gradient-descent
   (sampling-obj (l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))
