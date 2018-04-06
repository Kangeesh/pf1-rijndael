#lang racket
(require racket/base)
(provide (all-defined-out))

; ------------------------------------------------------- 
; AddRoundKey

;(define key1
;  '(1 2 3 4 5 6 7 8 9))

;(define key2
;  '(1 2 3 4 5 6 7 8 9))


; List<Bytes> List<Symbols> -> List<Bytes>
; given a list of bytes and a key, returns
; a list with the result of the XOR of the two
(define (do-xor byte-list key-section)
  (cond
    [(or (empty? byte-list) (empty? key-section)) '()]
    [else (cons (bitwise-xor (first byte-list) (first key-section)) (do-xor (rest byte-list) (rest key-section)))]
    )
  )
;(display "XOR of one list and a key:\n")
;(do-xor key1 key2)


; List<List> List<Symbols> -> List<Bytes>
; given a List of 4 lists and a key,
; returns a list with the bitwise XOR of the two
(define (AddRoundKey four-lists key)
  (cond
    [(or (empty? four-lists) (empty? key)) '()]
    [else (cons (do-xor (first four-lists) (first key)) (AddRoundKey (rest four-lists) (rest key)))]
    )
  )

;(display "XOR between a list of four elements and a key:\n")
;(AddRoundKey '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))


