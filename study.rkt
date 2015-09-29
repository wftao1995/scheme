#lang racket/load
(load "e:\\mysimply.rkt")

(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (rotate wd)
  (word (butfirst wd) (first wd)))

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (rotate wd))))

(define (choices menu)
  (if (null? menu)
      '(())
      (let ((smaller (choices (cdr menu))))
        (reduce append
                (map (lambda (item) (prepend-every item smaller))
                     (car menu))))))

(define (prepend-every item lst)
  (map (lambda (choice) (se item choice)) lst))

(define (combinations size set)
  (cond ((= size 0) '(()))
        ((empty? set) '())
        (else (append (prepend-every (first set)
                                     (combinations (- size 1)
                                                   (butfirst set)))
                      (combinations size (butfirst set))))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

