#lang racket

(define tcurry
  (λ (f . c)
    (λ x (apply f (append c x)))))