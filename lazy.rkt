#lang lazy

(provide !!)

;copied from slides
(provide take)
(define (take n L)
(if (or (zero? n) (empty? L))
empty
(cons (first L) (take (sub1 n) (rest L)))))

;infinite list
(provide f-list)
(define f-list 
    (cons 1 (cons 2 (cons 3 (map fun f-list (cdr (cdr f-list))))))
)

;function to get value
(provide fun)
(define (fun a b)
    (+ (* 3 b) (* 2 a))
)