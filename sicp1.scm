;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sicp1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (sqrt-i x)
  (sqrt-iter x 1))

(define square (λ (x) (* x x)))

(define (sqrt-iter x guess)
  (if (good-enough? x guess)
      guess
      (sqrt-iter  x (improve guess x))))

(define (improve guess x)
  ((λ (x y) (/ (+ x y) 2)) guess (/ x guess)))

(define (good-enough? x guess)
  (< (abs (- (square guess) x)) 0.0000001))
