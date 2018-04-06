#lang racket
(provide (all-defined-out))

; Converting a string into a byte string.
; A Byte String is a fixed-length array of bytes.
;(define schneier #"There are two kinds of cryptography in this world:
;cryptography that will stop your kid sister from reading your files,
;and cryptography that will stop major governments from reading your files.
;This book is about the latter. If I take a letter, lock it in a safe,
;hide the safe somewhere in New York, and then tell you to read the letter,
;that's not security. That's obscurity. On the other hand, if I take a letter
;and lock it in a safe, and then give you the safe along with the
;design specifications of the safe and a hundred identical safes with their
;combinations so that you and the world's best safecrackers can study the
;locking mechanism - and you still can't open the safe and read the letter, that's security.")
;
;(define schneier1 #"123456781234567912345678123456771234567812345670
;123456781234567912345678123456771234567812345670
;123456781234567912345678123456771234567812345670
;123456781234567912345678123456771234567812345670")

; Returns the length of the list, once converted the string.
;(define schneier-ll
;  (length (bytes->list schneier)))

; N String -> List-of-strings 
; creates a list of n copies of s
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))


; Number -> Number
; Given the length of the list from the text converted,
; returns the number of necessary padding to be divisible in 16 Bytes matrixes.
(define (padding length-list)
  (if (zero? (modulo length-list 16))
      length-list
     (padding (add1 length-list))))

;(display "\nPadding? If yes, so much: ")
;(zero? (modulo (padding schneier-ll) 16))

; Byte String -> List
; Given a byte string, returns the list of UNICODE decimals and padding.
(define (bstr-list txt)
  (if (zero? (modulo (length (bytes->list txt)) 16))
      (bytes->list txt)
      (append (bytes->list txt)
              (copier (- (padding (length (bytes->list txt))) (length (bytes->list txt)))
                      32))))

; Returns the list of the text plus the necessary padding
;(display "\nList and padding: ")
;(bstr-list schneier)


; Returns the whole list, converted
;(display "\nNumber of elements: ")
;(length (bstr-list schneier))
;(display "\nNumber of rows ")
;(/ (length (bstr-list schneier)) 4)
;(display "\nNumber of matrixes ")
;(/ (length (bstr-list schneier)) 16)

; (modulo (length (bstr-list schneier)) 128)
; (bytes->list schneier)
; (display "Conversion to UNICODE decimals:\n")
; (bstr-list schneier)

; List -> List
; Given a list of 32bit blocks, it divides it in 8bits blocks size.
(define (row-matrix ls)
  (cond [(= (length ls) 0) '()]
        [else (cons (take ls 4)
                    (row-matrix (drop ls 4)))])) 

; List Number -> List
; Given a list, it divides it in 128bit block size.
(define (128bits-matrix ls)
  (cond [(= (length ls) 0) '()]
        [else (cons (take ls 4) ; It takes values in groups of 4 rows each...
                    (128bits-matrix (drop ls 4)))])) ; ...dividing each rown in 4 lists.

; List -> List
(define (matrix-list ls)
  (128bits-matrix (row-matrix (bstr-list (string->bytes/latin-1 ls))))
  )

(define (matrix-string matrix list)
  (cond [(empty? matrix) (bytes->string/latin-1 (list->bytes list))]
        [(not (list? (first (first matrix)))) (matrix-string (rest matrix) (append list (first matrix)))]
        [(matrix-string (append (rest (first matrix)) (rest matrix)) (append list (first (first matrix))))]))

;(matrix-list "ciao")
;(matrix-string '(((99 105 97 111) (32 32 32 32) (32 32 32 32) (32 32 32 32))) '())

;(display "\nNumber of matrixes: ")
;(/ (length (div-matrixes (bstr-list schneier))) 16)
;
;
;(display "\nRows from the list:\n")
;(row-matrix (bytes->list schneier))
;
;(display "\nFirst Matrix:\n")
;(take (row-matrix (bytes->list schneier)) 4)
;
;(display "\nSecond Matrix:\n")
;(take (drop (row-matrix (bytes->list schneier)) 4) 4)
;
;(display "\nThird Matrix:\n")
;(take (drop (row-matrix (bytes->list schneier)) 8) 4)
;
;(display "\nAll matrixes without padding:\n") 
;(div-matrixes (row-matrix (bytes->list schneier)))
;(display "\nAll matrixes without padding:\n") 
;(div-matrixes (row-matrix (bstr-list schneier)))