#lang racket
(require malt)


#;
(define line
  (λ (x)
    (λ (w b)
      (let ([y (+ (* w x) b)])
        y))))

#;
(define line
  (λ (x)
    (λ (w b)
      (+ (* w x) b))))

(define line
  (λ (x)
    (λ (θ)
      (+ (* (ref θ 0) x)
         (ref θ 1)))))

((line 5) (list 2.5 2.2))
