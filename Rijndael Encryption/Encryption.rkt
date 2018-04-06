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
(define (first-round input key)
  (AddRoundKey input key)
  )

; 2nd-9th round
; 1. SubByte - (SubByte matrix)
; 2. ShiftRows - (ShiftRows four-lists n), n always initialized 0
; 3. MixColumns - (MixColumns matrix)
; 4. AddRoundKey - (AddRoundKey four-lists key)

(define (main-round input key)
  (AddRoundKey (MixColumns (ShiftRows (SubByte input) 0)) key)
  )

; Final round
(define (final-round input-text key10)
  (AddRoundKey (ShiftRows (SubByte input-text) 0) key10))


(define (Rijndael r input key)
  (cond
    [(equal? r 10) (final-round input (GenerateRoundKey r 0 key))]
    [(equal? r 0) (Rijndael (add1 r) (first-round input key) key)]
    [else (Rijndael (add1 r) (main-round input (GenerateRoundKey r 0 key)) key)]
    )
  )

(define (do-encryption input key)
  (cond
    [(empty? input) '()]
    [else (cons (Rijndael 0 (first input) key)
                (do-encryption (rest input) key))]
    )
  )

(define test-key (transpose 0 (128bits-matrix '(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f))))
(define test-input (transpose 0 (128bits-matrix '(#x00 #x11 #x22 #x33 #x44 #x55 #x66 #x77 #x88 #x99 #xaa #xbb #xcc #xdd #xee #xff))))
(define test-result (transpose 0 (128bits-matrix '(#x69 #xc4 #xe0 #xd8 #x6a #x7b #x04 #x30 #xd8 #xcd #xb7 #x80 #x70 #xb4 #xc5 #x5a))))
(define key1 (transpose 0 (128bits-matrix '(#xd6 #xaa #x74 #xfd #xd2 #xaf #x72 #xfa #xda #xa6 #x78 #xf1 #xd6 #xab #x76 #xfe))))
(define key2 (transpose 0 (128bits-matrix '(#xb6 #x92 #xcf #x0b #x64 #x3d #xbd #xf1 #xbe #x9b #xc5 #x00 #x68 #x30 #xb3 #xfe))))
(define key3 (transpose 0 (128bits-matrix '(#xb6 #xff #x74 #x4e #xd2 #xc2 #xc9 #xbf #x6c #x59 #x0c #xbf #x04 #x69 #xbf #x41))))
(define key4 (transpose 0 (128bits-matrix '(#x47 #xf7 #xf7 #xbc #x95 #x35 #x3e #x03 #xf9 #x6c #x32 #xbc #xfd #x05 #x8d #xfd))))
(define key5 (transpose 0 (128bits-matrix '(#x3c #xaa #xa3 #xe8 #xa9 #x9f #x9d #xeb #x50 #xf3 #xaf #x57 #xad #xf6 #x22 #xaa))))
(define key6 (transpose 0 (128bits-matrix '(#x5e #x39 #x0f #x7d #xf7 #xa6 #x92 #x96 #xa7 #x55 #x3d #xc1 #x0a #xa3 #x1f #x6b))))
(define key7 (transpose 0 (128bits-matrix '(#x14 #xf9 #x70 #x1a #xe3 #x5f #xe2 #x8c #x44 #x0a #xdf #x4d #x4e #xa9 #xc0 #x26))))
(define key8 (transpose 0 (128bits-matrix '(#x47 #x43 #x87 #x35 #xa4 #x1c #x65 #xb9 #xe0 #x16 #xba #xf4 #xae #xbf #x7a #xd2))))
(define key9 (transpose 0 (128bits-matrix '(#x54 #x99 #x32 #xd1 #xf0 #x85 #x57 #x68 #x10 #x93 #xed #x9c #xbe #x2c #x97 #x4e))))
(define key10 (transpose 0 (128bits-matrix '(#x13 #x11 #x1d #x7f #xe3 #x94 #x4a #x17 #xf3 #x07 #xa7 #x8b #x4d #x2b #x30 #xc5))))
(define r1 (transpose 0 (128bits-matrix '(#x89 #xd8 #x10 #xe8 #x85 #x5a #xce #x68 #x2d #x18 #x43 #xd8 #xcb #x12 #x8f #xe4))))
(define r2 (transpose 0 (128bits-matrix '(#x49 #x15 #x59 #x8f #x55 #xe5 #xd7 #xa0 #xda #xca #x94 #xfa #x1f #x0a #x63 #xf7))))
(define r3 (transpose 0 (128bits-matrix '(#xfa #x63 #x6a #x28 #x25 #xb3 #x39 #xc9 #x40 #x66 #x8a #x31 #x57 #x24 #x4d #x17))))
(define r4 (transpose 0 (128bits-matrix '(#x24 #x72 #x40 #x23 #x69 #x66 #xb3 #xfa #x6e #xd2 #x75 #x32 #x88 #x42 #x5b #x6c))))
(define r5 (transpose 0 (128bits-matrix '(#xc8 #x16 #x77 #xbc #x9b #x7a #xc9 #x3b #x25 #x02 #x79 #x92 #xb0 #x26 #x19 #x96))))
(define r6 (transpose 0 (128bits-matrix '(#xc6 #x2f #xe1 #x09 #xf7 #x5e #xed #xc3 #xcc #x79 #x39 #x5d #x84 #xf9 #xcf #x5d))))
(define r7 (transpose 0 (128bits-matrix '(#xd1 #x87 #x6c #x0f #x79 #xc4 #x30 #x0a #xb4 #x55 #x94 #xad #xd6 #x6f #xf4 #x1f))))
(define r8 (transpose 0 (128bits-matrix '(#xfd #xe3 #xba #xd2 #x05 #xe5 #xd0 #xd7 #x35 #x47 #x96 #x4e #xf1 #xfe #x37 #xf1))))
(define r9 (transpose 0 (128bits-matrix '(#xbd #x6e #x7c #x3d #xf2 #xb5 #x77 #x9e #x0b #x61 #x21 #x6e #x8b #x10 #xb6 #x89))))

;(display "text:\n")
;(do-encryption (matrix-list #"silence like a cancer grows") test-key)

;(Rijndael 0 (128bits-matrix (bstr-list #"abcdefghijklmnop")) (128bits-matrix (bstr-list #"ponmlkjihgfedcba")))
;(display "test run:\n")
;(Rijndael 0 test-input test-key)
;(display "expected:\n")
;test-result

;
;(display "first round\n")
;(AddRoundKey test-input test-key)
;(display "main round\n")
;(main-round test-input test-key)
;(display "final round\n")
;(final-round test-input test-key)
 #|
(GenerateRoundKey 0 0 test-key)
(GenerateRoundKey 1 0 test-key)
(GenerateRoundKey 2 0 test-key)
(GenerateRoundKey 3 0 test-key)
(GenerateRoundKey 4 0 test-key)
(GenerateRoundKey 5 0 test-key)
(GenerateRoundKey 6 0 test-key)
(GenerateRoundKey 7 0 test-key)
(GenerateRoundKey 8 0 test-key)
(GenerateRoundKey 9 0 test-key)
(GenerateRoundKey 10 0 test-key)
|#




