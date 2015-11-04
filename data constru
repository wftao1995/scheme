#lang racket
(define (squen seq)
  (let ((l '()))
    (define (flt seq)
      (cond ((equal? seq '()) empty)
            ((pair? (first seq)) (append l (flt (first seq)) (flt (rest seq))))
            (else (append (list (first seq) (flt (rest seq)))))))
    (flt seq)))


(define (flatten seq)
  (let ((l '()))
    (define (flt seq)
      (cond ((equal? seq '()) empty)
            ((pair? (first seq)) (append l (flt (first seq)) (flt (rest seq))))
            (else (append (list (first seq)) (flt (rest seq))))))
    (flt seq)))