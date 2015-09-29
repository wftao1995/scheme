#lang racket
(define (scientific x y)
  (floor (* (exp (* (log 10)
                    y))
            x)))

(define (sci-expoent x)
  (floor (/ (log x) (log 10))))

(define (lg x)
  (/ (log x) (log 10)))

(define (expbyten x)
  (exp (* (log 10) x)))

(define (sci-coefficient x)
  (exp (* (log 10)
          (- (/ (log x) (log 10)) (floor (/ (log x) (log 10)))))))

(define (sci x)
  (/ x (expbyten (sci-expoent x))))

(define (discount x y)
  (* x 
     (- 1 
        (/ y 
           100))))

(define (tip x)
  (- (ceiling (+ x
                 (* x 0.15)))
     x))