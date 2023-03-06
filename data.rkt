#lang racket
(require malt)
(provide (all-defined-out))


(define line-xs [tensor 2.0 1.0 4.0 3.0])
(define line-ys [tensor 1.8 1.2 4.2 3.3])

(define quad-xs (tensor -1.0 0.0 1.0  2.0  3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

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
