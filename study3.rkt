#lang scheme/load
(load "/home/atao/lisp/mysimply.rkt")

(define (roots a b c)
  (let ((discriminant (sqrt (- (* b b) (* 4 a c)))))
    (se (/ (+ (- b) discriminant) (* 2 a))
        (/ (- (- b) discriminant) (* 2 a)))))

(define (roots2 a b c)
  (let ((discriminant (sqrt (- (* b b) (* 4 a c))))
        (minus-b (- b))
        (two-a (* 2 a)))
    (se (/ (+ minus-b discriminant) two-a)
        (/ (- minus-b discriminant) two-a))))

(define (notice1 n)
  (let ((a (+ 4 n)))
    (let ((b (* a 5)))
      (+ a b))))
;;不能
;;(let ((a (+ 4 n))
;;      (b (* a 5)))
;;  (+ a b))