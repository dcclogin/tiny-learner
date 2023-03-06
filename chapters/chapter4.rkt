#lang racket
(require malt)

;; Slip-slidin' away

;; gradient: rate of change of a parameterized function wrt ALL parameters...

(∇ (λ (θ) (sqr (ref θ 0))) (list 27))
(∇ (λ (θ) (+ (sqr (ref θ 0))
             (sqr (ref θ 1))))
   (list 2 3))


(define l2-loss
  (λ (target)
    (λ (xs ys)  ;; ⇐ expectant function
      (λ (θ)    ;; ⇐ objective function
        (let ([ys* ((target xs) θ)])
          (sum (sqr (- ys ys*))))))))

(let ([xs (tensor 2.0 1.0 4.0 3.0)]
      [ys (tensor 1.8 1.2 4.2 3.3)]
      [θ (list 0.0 0.0)])
  (let ([obj ((l2-loss line) xs ys)])
    (∇ obj θ)))


;; iteration of revision function f
(define revise
  (λ (f n θ)
    (cond
      [(zero? n) θ]
      [else (revise f (sub1 n) (f θ))])))


(let ([f (λ (θ)
           (map (λ (p) (- p 3)) θ))])
  (revise f 5 (list 1 2 3)))



;; the first "learning" process! (optimization by gradient descent)
(let ([α 0.01]
      [xs (tensor 2.0 1.0 4.0 3.0)]
      [ys (tensor 1.8 1.2 4.2 3.3)])
  (let ([obj ((l2-loss line) xs ys)])
    (let ([f (λ (θ)
               (let ([gs (∇ obj θ)])
                 (list (- (ref θ 0) (* α (ref gs 0)))
                       (- (ref θ 1) (* α (ref gs 1))))))])
      (revise f 1000 (list 0.0 0.0)))))

;; improve f so that it doesn't rely on the length of θ
(let ([α 0.01]
      [xs (tensor 2.0 1.0 4.0 3.0)]
      [ys (tensor 1.8 1.2 4.2 3.3)])
  (let ([obj ((l2-loss line) xs ys)])
    (let ([f (λ (θ)
               (let ([gs (∇ obj θ)])
                 (map (λ (p q) (- p (* α q)))
                      θ
                      gs)))])
      (revise f 1000 (list 0.0 0.0)))))

;; Question: why we need 1000?



;; abstract the whole learning process
(define α 0.01)
(define revs 1000)
(define gradient-descent
  (λ (obj θ)
    (let ([f (λ (Θ)
               (let ([gs (∇ obj Θ)])
                 (map (λ (p q) (- p (* α q)))
                      Θ
                      gs)))])
      (revise f revs θ))))

(let ([xs (tensor 2.0 1.0 4.0 3.0)]
      [ys (tensor 1.8 1.2 4.2 3.3)])
  (gradient-descent
   ((l2-loss line) xs ys)
   (list 0.0 0.0)))
