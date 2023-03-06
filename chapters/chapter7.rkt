#lang racket
(require malt)

#|

-- The Law of Revision --

As long as we make sure that gradient-descent accepts an initial θ
and results in a well-fitted θ, any reasonable way of revising it
from the first to the last revision is okay.

|#

(declare-hyper revs)
(declare-hyper α)

;; i for inflate
;; θ → [Θ]
(define lonely-i^
  (λ (θ)
    (map (λ (p) (list p)) θ)))

;; d for deflate
;; [Θ] → θ
(define lonely-d^
  (λ (Θ)
    (map (λ (P) (ref P 0)) Θ)))

;; u for update
;; [Θ] × θ → [Θ]
(define lonely-u^
  (λ (Θ gs)
    (map (λ (P g)
           (list (- (ref P 0) (* α g))))
         Θ
         gs)))



(define gradient-descent^
  (λ (inflate^ deflate^ update^)
    (λ (obj θ)
      (let ([f (λ (Θ)
                 (update^ Θ
                         (∇ obj (deflate^ Θ))))])
        (deflate^
          (revise f revs (inflate^ θ)))))))

(define lonely-gradient-descent^
  (gradient-descent^
   lonely-i^
   lonely-d^
   lonely-u^))


(define naked-i^
  (λ (θ)
    (map (λ (p)
           (let ([P p])
             p))
         θ)))

(define naked-d^
  (λ (Θ)
    (map (λ (P)
           (let ([p P])
             p))
         Θ)))

(define naked-u^
  (λ (Θ gs)
    (map (λ (P g)
           (- P (* α g)))
         Θ
         gs)))

(define naked-gradient-descent^
  (gradient-descent^
   naked-i^
   naked-d^
   naked-u^))

;; the ultimate version!
(define gradient-descent
  (λ (inflate deflate update)
    (λ (obj θ)
      (let ([f (λ (Θ)
                 (map update
                      Θ
                      (∇ obj (map deflate Θ))))])
        (map deflate
             (revise f revs (map inflate θ)))))))


(define lonely-i
  (λ (p)
    (list p)))
(define lonely-d
  (λ (P)
    (ref P 0)))
(define lonely-u
  (λ (P g)
    (list (- (ref P 0) (* α g)))))

(define lonely-gradient-descent
  (gradient-descent
   lonely-i
   lonely-d
   lonely-u))


(define naked-i
  (λ (p)
    (let ([P p])
      P)))
(define naked-d
  (λ (P)
    (let ([p P])
      p)))
(define naked-u
  (λ (P g)
    (- P (* α g))))

(define naked-gradient-descent
  (gradient-descent
   naked-i
   naked-d
   naked-u))
