#lang racket
(require racket/base)
(require "Utilities.rkt")
(require "SubByte.rkt")
(require "ShiftRows.rkt")
(provide (all-defined-out))

; ------------------------------------------------------- 
; Key Expansion
(define sample-key
  '((#x2B #x28 #xAB #x09) (#x7E #xAE #xF7 #xCF) (#x15 #xD2 #x15 #x4F) (#x16 #xA6 #x88 #x3C))
  )

; ------------


(define Rcon
  '( (#x01  #x02  #x04  #x08  #x10  #x20  #x40  #x80  #x1B  #x36)
     (#x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00)
     (#x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00)
     (#x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00))
  )

; STEP 1
; take the last elements of the 16-byte block (see step 4 for
; the other formula

;(get-column 3 sample-key)


; STEP 2
; take the first element and put it on the
; 4th position pushing the other elements up
; --> like ShiftRows

;(do-shift 1 (get-column 3 sample-key))


; STEP 3
; do SubByte

(define (mixed-column-key sample-key)
  (SubByte-list (do-shift (get-column 3 sample-key) 1))
  )

;(display "4th column of sample key mixed:\n")
;(mixed-column-key sample-key)

; STEP 4
; XOR between the result of previous SubByte, the first column
; of the key and the Round constant word array (Rcon)


;(bitwise-xor #x2B #x8A #x01)
;(bitwise-xor 43 138 1)

;(display "first column of the key and Rcon:\n")
;(get-column 0 sample-key)
;(get-column 0 Rcon)

(define (first-column-new-key current-round n sample-key)
  (cond
    [(equal? n 4) '()]
    [else (cons
           (bitwise-xor (nth-element n (mixed-column-key sample-key))
                        (nth-element n (get-column 0 sample-key))
                        (nth-element n (get-column current-round Rcon)))
           (first-column-new-key current-round (add1 n) sample-key))]
    )
  )
;(display "first column of the new key:\n")
;(first-column-new-key 0 sample-key Rcon)

; generate the remaining
; 3 columns starting from the newly created
; first column of the new key
;(define (remaining-columns-new-key sample-key new-column)

(define (next-column n previous-key current-partial-key)
  (local ((define previous-column (nth-element (sub1 (length current-partial-key)) current-partial-key))
          (define column-number (length current-partial-key)))
    (cond [(equal? n 4) '()]
        [else (cons (bitwise-xor (nth-element n (get-column column-number previous-key))
                                 (nth-element n previous-column))
                    (next-column (add1 n) previous-key current-partial-key))]
        )
    )
  )

;(display "second column of the new key:\n")
;(next-column 0 sample-key (list (first-column-new-key 0 sample-key Rcon)))

(define (NextRoundKey current-round previous-key new-key)
  (cond [(empty? new-key) (NextRoundKey current-round previous-key (list (first-column-new-key current-round 0 previous-key)))]
        [(equal? (length new-key) 4) (transpose 0 new-key)]
        [(NextRoundKey current-round previous-key
                       (append new-key
                               (list (next-column 0 previous-key new-key))))]
        )
  )


;(display "1st NextRoundKey\n")
;(NextRoundKey sample-key '())
;(display "2nd NextRoundKey\n")
;(NextRoundKey (NextRoundKey sample-key '()) '())
;(display "next column\n")
;(next-column (length '((160 250 254 23))) 0 sample-key (nth-element (sub1 (length '((160 250 254 23)))) '((160 250 254 23))))
;(append '((160 250 254 23))
;        (list (next-column (length '((160 250 254 23))) 0 sample-key (nth-element (sub1 (length '((160 250 254 23)))) '((160 250 254 23)))))
;        )

(define (GenerateRoundKey round current-round previous-key)
  (cond [(equal? round current-round) previous-key]
        [else (GenerateRoundKey round (add1 current-round) (NextRoundKey current-round previous-key '()))]))

;(display "1st NextRoundKey\n")
;(GenerateRoundKey 1 0 sample-key)
;(display "10th NextRoundKey\n")
;(GenerateRoundKey 10 0 sample-key)


(define (AllRounds generated-round-keys)
  (cond [(equal? (length generated-round-keys) 10) generated-round-keys]
        [else (AllRounds (append generated-round-keys
                                 (list (NextRoundKey (length generated-round-keys) (nth-element (sub1 (length generated-round-keys)) generated-round-keys) '()))))]
        )
  )

;(display "All round keys\n")
;(AllRounds (list sample-key))

;(nth-element 1 (AllRounds (list sample-key)))
;(nth-element 10 (AllRounds (list sample-key)))