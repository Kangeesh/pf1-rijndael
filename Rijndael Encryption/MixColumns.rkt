#lang racket
(require math/base)
(require "Utilities.rkt")
(provide (all-defined-out))

; MixColumns

(define circulant-MDS
  '((02 03 01 01)
    (01 02 03 01)
    (01 01 02 03)
    (03 01 01 02)))

; Inversion MixCOlumns

(define fixed-polynomial
  '((#x0E #x0B #x0D #x09)
    (#x09 #x0E #x0B #x0D)
    (#x0D #x09 #x0E #x0B)
    (#x0B #x0D #x09 #x0E)))


;(display "\nDivision in columns:\n")
;(transpose 0 ex-ls)
 
;; Example taken from AESmatlab.pdf --- CORRECT!!!
;; Result: '((95 87 247 29) (114 245 190 185) (100 188 59 249) (21 146 41 26))
;(define ex-ls3
;  '((#x63 #x09 #xcd #xba) (#x53 #x60 #x70 #xca) (#xe0 #xe1 #xb7 #xd0) (#x8c #x04 #x51 #xe7)))
;
;; Example from the video... the result is wrong! Here is correct... --- CORRECT!!!
;(define ex-ls2
;  '((#xD4 #xBF #x5D #x30) (#xE0 #xB4 #x52 #xAE) (#xB8 #x41 #x11 #xF1) (#x1E #x27 #x98 #xE5)))
;
;; ...Another example from Google Images, search "mixcolums". --- CORRECT!!!
;; Result:'((71 64 163 76) (55 212 112 159) (148 228 58 66) (237 165 166 188))

;(define ex-ls
;  '((#x87 #xF2 #x4D #x97) (#x6E #x4C #x90 #xEC) (#x46 #xE7 #x4A #xC3) (#xA6 #x8C #xD8 #x95)))
;
;(define ex-ls1
;  '((101 32 97 114) (101 32 116 119) (111 32 116 121) (112 101 115 32)))
;
;(display "\nGiven this list:\n")
;ex-ls


; ----------------------------------------------------------------------------------------
; Content:..........A package for generating QR codes according to the JIS X 0510 spec
; Package owner:....norisys
; Source code:......PlaneT > norisys > qrcode.plt > private > galois.rkt
; Notes:............Original version the GF Multiplication used m(x)=(x^8+x^4+x^3+x^2+1)
; ................. but in Rijndael is used m(x)=(x^8 + x^4 + x^3 + x + 1).
; Website link:..........................................................................
; https://planet.racket-lang.org/package-source/norisys/qrcode.plt/1/0/private/galois.rkt
; ----------------------------------------------------------------------------------------

; Number Number -> Number
; Multiply two polynomials using the peasant's algoithm, reducing
; modulo (x^8 + x^4 + x^3 + x + 1)
(define (gf* a b)
  (let ([m #x11B])
    (do ([x 0 (if (bitwise-bit-set? b 0)
                  (bitwise-xor x a)
                  x)]
         [a a (if (bitwise-bit-set? a 7)
                  (bitwise-xor (arithmetic-shift a 1) m)
                  (arithmetic-shift a 1))]
         [b b (arithmetic-shift b -1)])
      ((zero? b) (bitwise-and x #xFF)))))
; ----------------------------------------------------------------------------------------

; List<Numbers> List<Numbers> -> Number
; multiplication of the first elements of two lists
(define (mul-elements list1 list2)
  (gf* (first list1) (first list2)))

;(display "\nMultiplication:  \n")
;(mul-elements '(#xD4 #xBF #x5D #x30) '(1 2 3 4))

; List<Numbers> List<Numbers> -> List<Numbers>
; given two lists, multiplies all elements of one list with the number
; in their same position on the second list
(define (mul-all-elements list1 list2)
  (cond
    [(or (empty? list1) (empty? list2)) '()]
    [else (cons (gf* (first list1) (first list2))
                (mul-all-elements (rest list1) (rest list2)))]))

;(display "\nMultiplication between elements of two columns\n")
;(mul-all-elements '(1 2 3 4) '(2 2 2 2))
;(mul-all-elements '(101 32 97 114) '(2 3 1 1))

; List<Numbers> List<Numbers> -> List<Numbers>
; given two lists returns a list containing
; the multiplication between of all the pairs
; of the lists
(define (mul-matrix list1 list2)
  (cond
    [(or (empty? list1) (empty? list2)) '()]
    [else (cons (mul-all-elements (first list1) list2)
                (mul-matrix (rest list1) list2))]))

;(display "\nMultiplication between the first column and MDS.\n")
;(mul-matrix '((2 3 1 1) (1 2 3 1) (1 1 2 3) (3 1 1 2)) '(#xD4 #xBF #x5D #x30))

; List<Numbers> -> Number
; Given a column, returns the bitwise xor between the values.
(define (xor-operation list)
  (cond [(empty? list) 0]
        [else (bitwise-xor (first list) (xor-operation (rest list)))]))

; List<Numbers> List<Numbers> -> List<Numbers>
; given two lists, applies the function mul-matrix
; and returns the sum of the elements contained
; in every element-list of the resulting list
(define (xor-matrix list1 list2)
  (cond
    [(or (empty? list1) (empty? list2)) '()]
    [else (cons (xor-operation (mul-all-elements (first list1) list2))
                (xor-matrix (rest list1) list2))]))

;(display "\nBitwise XOR between all the values:\n")
;(xor-matrix circulant-MDS (get-column 0 ex-ls))
;(xor-matrix circulant-MDS (get-column 1 ex-ls))
;(xor-matrix circulant-MDS (get-column 2 ex-ls))
;(xor-matrix circulant-MDS (get-column 3 ex-ls))

; define a function for the multiplication and
; the sum for all the matrix
(define (mix-matrix n matrix1 matrix2)
  (cond [(equal? n 4) '()]
        [else (cons (xor-matrix matrix2 (get-column n matrix1))
                    (mix-matrix (add1 n) matrix1 matrix2))]))

;(display "mix-matrix:\n")
;(mix-matrix 0 ex-ls circulant-MDS)
;
(define (MixColumns matrix)
  (transpose 0 (mix-matrix 0 matrix circulant-MDS)))

(define (InvMixColumns matrix)
  (transpose 0 (mix-matrix 0 matrix fixed-polynomial)))
;
;(display "Original Matrix:\n")
;ex-ls
;
;(display "MixColumns:\n")
;(MixColumns ex-ls)

;(InvMixColumns ex-ls)