#lang racket
(provide (all-defined-out))

; extract the nth element of the list and
; its corresponding value in a list
(define (nth-element n list)
  (cond [(equal? n 0) (first list)]
        [else (nth-element (sub1 n) (rest list))]))

; applies the general function nth-element to
; the wanted case
(define (substitution-byte original-byte sub-box)
  (nth-element original-byte sub-box))

; List<Number> Number -> List<Number>
; get the element of the n-th column
; e.g. get the first elements of the lists
(define (get-column n list)
  (cond
    [(empty? list) '()]
    [else (cons (nth-element n (first list)) (get-column n (rest list)))]))

; List<Numbers> Number -> List<Numbers>
; a function that takes a list of lists and returns
; a list of lists with the elements of the original
; list organized depending on their position
; e.g. '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))
; -->  '((1 5 9 13) (2 6 10 14) (3 7 11 15) (4 8 12 16))
(define (transpose n lists)
  (cond
    [(equal? n 4) '()]
    [else (cons (get-column n lists) (transpose (+ n 1) lists))]))