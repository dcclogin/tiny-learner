#lang racket
(require malt)

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

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

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


(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))
(define plane-ys
  (tensor 13.99
          15.99
          18.0
          22.4
          30.2
          37.94))
(with-hypers
  ([revs 15000]
   [α 0.001]
   [batch-size 4])
  (gradient-descent
   (sampling-obj (l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))
