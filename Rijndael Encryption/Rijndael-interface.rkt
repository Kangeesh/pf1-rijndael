#lang racket
(require "Matrix.rkt")
(require "Utilities.rkt")
(require "Encryption.rkt")
(require "Decryption.rkt")
(provide (all-defined-out))


(define (final-encryption input-string key-string)
  (matrix-string (do-encryption (matrix-list input-string) (first (matrix-list key-string))) '()))

(define (final-decryption input-string key-string)
  (matrix-string (do-decryption (matrix-list input-string) (first (matrix-list key-string))) '()))

;(display "final encryption:\n")
;(final-encryption "Hello World" "adminadmin")
;(display "final decryption:\n")
;(final-decryption "\u001Có\u0093M§âLÙTrVtý\bÚÿ" "adminadmin")
