#lang racket

;1
(provide last)
(define (last a L)
    (local
        (
            (define (fun L i j)
                (cond
                    [(null? L) i] ;return index on end of list
                    [(eq? (first L) a) (fun (cdr L) j (+ j 1))] ;set last index of a and increment index
                    [else (fun (cdr L) i (+ j 1))] ;increment index if not seen a
                )
            )
        )
	(fun L -1 0) ;start recusion
    )
)

;2
(provide wrap)
(define (wrap M)
    (cond 
        [(null? M) M] ;return M when done
        [else (cons (cons (car M) null) (wrap (cdr M)))] ;wrap first attom and recurse
    )
)

;3
;for some reason did not like just calling itself for recursion so added the abstracted function
(provide count-parens-all)
(define (count-parens-all M)
    (local 
        (   
            (define (fun M)
                (cond
                    [(null? M) 2] ;return 2 on empty list
                    [(list? (car M)) (+ (fun (car M)) (fun (cdr M)))] ;recurse head and rest, add results
                    [else (fun (cdr M))] ;skip head if atom
                )
            )
        )
    (fun M)
    )
)

;4
(provide insert-right-all)
(define (insert-right-all new old M)
    (local 
        (   
            (define (fun M)
                (cond
                    [(null? M) M] ;pass back on empty list
                    [(list? (car M)) (cons (fun (car M)) (fun (cdr M)))] ;dig into list and continue with rest
                    [(eq? (car M) old) (cons (car M) (cons new (fun (cdr M))))] ;check if atam is the one we want, add new after it
                    [else (cons (car M) (fun (cdr M)))] ;keep digging into list
                )
            )
        )
    (fun M)
    )
)

;5
(provide invert)
(define (invert M)
    (cond
        [(null? M) M] ;empty list
        [(list? (car M)) (cons (invert (car M)) (invert (cdr M)))] ;go into list
        [else (append (cdr M) (cons (car M) null))] ;invert, cons was putting () around the first element
    )
)

;6
;realized its probably easier to just keep passing the static vars
;rather than turn them into local variables
(provide filter-out)
(define (filter-out pred L)
    (cond
        [(null? L) L] ;you get the idea
        [(pred (car L)) (filter-out pred (cdr L))]
        [else (cons (car L) (filter-out pred (cdr L)))]
    )
)

;7
(provide summatrices)
(define (summatrices M1 M2)
    (map (lambda (M1 M2) (map + M1 M2)) M1 M2) ;going off slides
)

;8
(provide swapper)
(define (swapper a1 a2 M)
    (cond
        [(null? M) M] ;same ol
        [(list? M) (cons (swapper a1 a2 (car M)) (swapper a1 a2 (cdr M)))]
        [else   (cond
                    [(eq? M a1) a2]
                    [(eq? M a2) a1]
                    [else M]
                )] ;on atom, swap if needed
    )
)

;9
(provide flatten)
(define (flatten M)
    (cond
        [(null? M) M]
        [(list? M)  (cond
                        [(null? (car M)) (flatten (cdr M))]
                        [else (append (flatten (car M)) (flatten (cdr M)))]
                    )] ;remove nulls
        [else (cons M null)] ;convert atoms to lists
    )
)

;10
(provide binary-tree-insert)
(define (binary-tree-insert T n)
    (local 
        (
            (define (fun L R N m)
                (cond
                    [(null? N) (cons null (cons m '(()) ))] ;leaf
                    [(> m N) (cons L (cons N (cons (binary-tree-insert (car R) m) null)))] ;right
                    [else (cons (cons (binary-tree-insert (car L) m) null) (cons N R))] ;left
                )
            )
        )
        (cond
            [(null? T) (fun null null null n)] ;leaf
            [else (fun (car T) (cdr(cdr T)) (car(cdr T)) n)]
        )
    )
)

;11
;this whole thing is just following the slides to a T
(provide abstractra)
(define (abstractra one two three)
    (local
        (
            (define (helper a M)
                (cond
                    [(null? M) (one)]
                    [(not (pair? (first M))) (two a M)]
                    [else (three a M)]
                )
            )
        )
        helper
    )
)

(provide rember*)
(define rember*
    (abstractra
        (lambda () '()) 
        (lambda (a M)
            (if (eq? a (first M)) 
                (rember* a (rest M))
                (cons (first M) (rember* a (rest M)))
            )
        ) 
        (lambda (a M)
            (cons (rember* a (first M)) (rember* a (rest M)))
        )
    )
)

(provide depth)
(define (depth M)
    (
        (abstractra
            (lambda () 1) 
            (lambda (a M) 
                (depth (rest M))
            ) 
            (lambda (a M) 
                (max (add1 (depth (first M))) (depth (rest M)))
            ) 
        ) 
    null M
    )
)