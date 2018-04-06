#lang racket
(require "Matrix.rkt")
(require "AddRoundKey.rkt")
(require "SubByte.rkt")
(require "ShiftRows.rkt")
(require "MixColumns.rkt")
(require "Utilities.rkt")
(require "KeyExpansion.rkt")
(provide (all-defined-out))


; keys for the 10 rounds:
; (AllRounds (list sample-key))

; First round
; AddRoundKey - (AddRoundKey four-lists key)
; Input - (128bits-matrix ls)
(define (first-round-dec input key)
  (AddRoundKey input key)
  )

; 2nd-9th round
; 1. InvShiftRows - (InvShiftRows four-lists)
; 2. InvSubBytes - (SubByte-list list-of-bytes) - (SubByte matrix)
; 3. AddRoundKey - (AddRoundKey four-lists key)
; 4. InvMixColumns - (MixColumns matrix)

(define (main-round-dec input key10)
  (InvMixColumns (AddRoundKey (InvSubByte (InvShiftRows input)) key10))
  )

; Final round
; InvShiftRows
; InvSubBytes
; AddRoundKey
(define (final-round-dec input key0)
  (AddRoundKey (InvSubByte (InvShiftRows input)) key0)
  )

(define (Rijndael-dec r input key)
  (cond
    [(equal? r 10) (final-round-dec input (GenerateRoundKey (- 10 r) 0 key))]
    [(equal? r 0) (Rijndael-dec (add1 r) (first-round-dec input (GenerateRoundKey (- 10 r) 0 key)) key)]
    [else (Rijndael-dec (add1 r) (main-round-dec input (GenerateRoundKey (- 10 r) 0 key)) key)]))

; recursively
(define (do-decryption input key)
  (cond
    [(empty? input) '()]
    [else (cons (Rijndael-dec 0 (first input) key)
                (do-decryption (rest input) key))]
    )
  )



;(define test-key (transpose 0 (128bits-matrix '(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f))))
;(define test-input (transpose 0 (128bits-matrix '(#x00 #x11 #x22 #x33 #x44 #x55 #x66 #x77 #x88 #x99 #xaa #xbb #xcc #xdd #xee #xff))))
;(define test-result (transpose 0 (128bits-matrix '(#x69 #xc4 #xe0 #xd8 #x6a #x7b #x04 #x30 #xd8 #xcd #xb7 #x80 #x70 #xb4 #xc5 #x5a))))
;
;(GenerateRoundKey (- 10 10) 0 test-key)
;(GenerateRoundKey (- 10 9) 0 test-key)
;(GenerateRoundKey (- 10 8) 0 test-key)
;(GenerateRoundKey (- 10 7) 0 test-key)
;(GenerateRoundKey (- 10 6) 0 test-key)
;(GenerateRoundKey (- 10 5) 0 test-key)
;(GenerateRoundKey (- 10 4) 0 test-key)
;(GenerateRoundKey (- 10 3) 0 test-key)
;(GenerateRoundKey (- 10 2) 0 test-key)
;(GenerateRoundKey (- 10 1) 0 test-key)
;(GenerateRoundKey (- 10 0) 0 test-key)