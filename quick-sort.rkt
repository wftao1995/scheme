#lang scheme
(define (q-sort l)
  (cond
    [(empty? l) empty]
    [(empty? (rest l)) (list (first l))]
    [else
     (let ((small-part (filter (lambda (x) (<= x (first l))) (rest l)))
           (big-part (filter (lambda (x) (> x (first l))) (rest l))))
       (append (q-sort small-part) (list (first l)) (q-sort big-part)))]
    )
  )
;;filter 过滤器，保留满足条件的 (filter fun list)
;;append 合并
;;rest buffirst