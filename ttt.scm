#lang scheme/load
(load "mysimply.rkt")
;;Tic-Tac-Toe
;;This function returns a "triple" of the board position according to the index
;(define (find-triples position)
;  (every substitute-triple '(123 456 789 147 258 369 159 357)))
(define (find-triples position)
  (every (lambda (comb)
           (substitute-triple comb position))
         '(123 456 789 147 258 369 159 357)))
;;
;(define (substitute-triple combination)
;  (accumulate word (every substitute-letter combination)))
(define (substitute-triple combination position)
  (accumulate word
              (every (lambda (square)
                            (substitute-letter square position))
                       combination)))
;;
(define (substitute-letter square position)
  (if (equal? '_ (item square position))
      square
      (item square position)))
;;
(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))
(define (opponent letter)
  (if (equal? letter 'x) 'o 'x))
;;
;(define (i-can-win? triples me)
;  (not (empty?
;        (keep (lambda (triple) (my-pair? triple me))
;              triples))))
(define (i-can-win? triples me)
  (choose-win
   (keep (lambda (triple) (my-pair? triple me))
         triples)))
;;Everything other than #f is count as true.
;;So we can write a prcedure that return the number of a winning move.
(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples))))
;;If the oppoent can win in the next move
(define (opponent-can-win? triples me)
  (i-can-win? triples (oppoent me)))

;;Now the strategy goes complicated
(define (i-can-fork? tripes me)
  (first-if-any (pivots triples me)))
(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me))
                          triples)))
(define (my-single? triple me)
  (and (= (appearances me triple) 1)
       (= (appearances (opponent me) triple) 0)))

(define (repeated-numbers sent)
  (every first
         (keep (lambda (wd) (>= (count wd) 2))
               (sort-digits (accumulate word sent)))))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (sort-digits number-word)
  (every (lambda (digit) (extract-digit digit number-word))
         '(1 2 3 4 5 6 7 8 9)))







(define (ttt-choose triples me)
  (cond ((i-can-win? triples me))
        ((opponent-can-win? triples me))
        ((i-can-fork? triples me))
        ((i-can-advance? triples me))
        (else (best-free-square triples))))
;考虑四个角落？

