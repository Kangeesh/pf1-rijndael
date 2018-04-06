#lang racket
(provide (all-defined-out))

; ------------------------------------------------------- 
; ShiftRows

; row 0: nothing changes
; row 1: first byte moves to 4th position
;        and push all to left:
;        - 1 -> 4
;        - 2 -> 1
;        - 3 -> 2
;        - 4 -> 3
; row 2: first and second byte move to the back
;        and push the other bytes to left:
;        - 1 -> 3
;        - 2 -> 4
;        - 3 -> 1
;        - 4 -> 2
; row 3: first, second and third byte move
;        to the back and push the 4th to the left:
;        - 1 -> 2
;        - 2 -> 3
;        - 3 -> 4
;        - 4 -> 1

; List<Bytes> -> List<Bytes>
; given a list of bytes, return the 4 rows shifted
; depending on the paramethers stated above
(define (do-shift list-bytes n)
  (append (drop list-bytes n) (take list-bytes n)))

; List<List> -> List<List>
; given a matrix (list of four elements/lists),
; return a new matrix with the rows modified
; [definition for a 4x4]

(define (ShiftRows four-lists n)
  (cond [(empty? four-lists) '()]
        [else (cons (do-shift (first four-lists) n) (ShiftRows (rest four-lists) (+ n 1)))]))

;(display "matrix:") (newline)
;(do-shift '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) 0)
;(display "do-shift 3:") (newline)
;(ShiftRows '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) 0)


; --------------------------------------------------------
; Inverse Shift Rows

; row 0: nothing changes
; row 1: last byte moves to 1st position
;        and push all to right:
;        - 1 -> 2
;        - 2 -> 3
;        - 3 -> 4
;        - 4 -> 1
; row 2: first and second byte move to the back
;        and push the other bytes to left:
;        - 1 -> 3
;        - 2 -> 4
;        - 3 -> 1
;        - 4 -> 2
; row 3: first, second and third byte move
;        to the back and push the 4th to the left:
;        - 1 -> 4
;        - 2 -> 1
;        - 3 -> 2
;        - 4 -> 3


; List<Bytes> -> List<Bytes>
; given a list of bytes, return the 4 rows shifted
; depending on the paramethers stated above
(define (do-shift-inv list-bytes n)
  (append (drop list-bytes n) (take list-bytes n)))

;(do-shift-inv 3 '(1 2 3 4))
;(do-shift-inv 2 '(1 2 3 4))
;(do-shift-inv 1 '(1 2 3 4))

; List<List> -> List<List>
; given a matrix (list of four elements/lists),
; return a new matrix with the rows modified
; [definition for a 4x4]

#|
(define (matrix four-lists n)
  (cond [(equal? n 4) '()]
        [(equal? n 0) (matrix (do-shift-inv (first four-lists) 0) (add1 n))]
        [else (cons (do-shift-inv (first four-lists) (- 4 n))
                    (matrix (rest four-lists) (add1 n)))]))
|#
(define (InvShiftRows four-lists)
  (cons (first four-lists)
               (cons (do-shift-inv (second four-lists) 3)
                     (cons (do-shift-inv (third four-lists) 2)
                           (cons (do-shift-inv (fourth four-lists) 1) empty)))))

; given a matrix with the rows shifted,
; return a matrix with its elements ordered as
; stated above.
;(define (InvShiftRows four-lists)
;  (append (drop (matrix four-lists 3) 3) (take (matrix four-lists 3) 3)))

;(display "shifted matrix:") (newline)
;(matrix '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) 3)
;(display "Matrix properly shifted\n") 
;(matrix '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))