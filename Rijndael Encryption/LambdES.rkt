; Philip Dinicastro
; Daniela Gianora

#lang racket/gui

(require 2htdp/image)
(require "Rijndael-interface.rkt")

; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::: Welcome page :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; Make a window of given dimensions and name.
(define entrance-frame (new frame%
                            [label "Lambda Encryption Standard"]
                            [width 600]	 
                            [height 600]))

; Box panel container for the message.
(define main-welcome-box (new group-box-panel%
                              [parent entrance-frame]
                              [label ""]
                              [horiz-margin 30]
                              [vert-margin 30]))

; Add a vertical panel to the dialog, with centering for buttons
(define welcome-panel (new vertical-panel%
                           [parent main-welcome-box]
                           [alignment '(center center)]
                           [horiz-margin 30]
                           [vert-margin 30]))

; Logo image on the welcome page.
(define logo (make-object bitmap%
               "logo1.png"
               'png/alpha))

; Message container for the image.
(define message-logo (new message%
                          [parent main-welcome-box]
                          [label logo]
                          [horiz-margin 30]))

; Box panel container for the message.
(define welcome-box (new group-box-panel%
                         [parent main-welcome-box]
                         [label "You have a new message!"]
                         [horiz-margin 30]
                         [vert-margin 30]))

; Message showing on the welcome page.
(define message-text (new message%
                          [parent welcome-box]
                          [label "Welcome to LambdES, a tool for encrypting and decrypting
your message using the AES 128 bits algorithm.
Click Start below to encrypt your lovely messages!"]))

; Start button for the application.
(new button%
     [parent main-welcome-box]
     [label "Start Encrypting Messages!"]
     [vert-margin 30]
     [callback (lambda (button event)
                 (send frame show #true)
                 (send entrance-frame show #false))])

; ---> Starts the Application
(send entrance-frame show #t)
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::: Application menu :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; New window frame for the application.
(define frame (new frame%
                   [label "AES 128 bits algorithm: encryption & decryption"]
                   [width 600]	 
                   [height 600]))

;; Box frame for the application components.
(define maingroup (new group-box-panel%
                       [label ""]
                       [parent frame]
                       [vert-margin 20]
                       [horiz-margin 5]
                       [alignment '(right center)]))

; Documentation button for the application.
(new button%
     [parent maingroup]
     [label "Documentation"]
     [callback (lambda (button event)
                 (send documentation show #true))]) ; see at the end for this frame.
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::: Encryption box :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Box frame for the application components.
(define maingroup-one (new group-box-panel%
                           [label "To encrypt a message, insert your text here:"]
                           [parent maingroup]
                           [vert-margin 20]
                           [horiz-margin 5]
                           [alignment '(left center)]
                           ))

;; Text Editor where to insert the text for encrypting and decrypting.
(define text-one (new text-field%
                      [parent maingroup-one]
                      [label ""]
                      [init-value ""]
                      [style '(multiple)]))

;; Add a text field to the dialog
(define encryption-key
  (new text-field%
       [parent maingroup-one]
       [label "Secret Key"]	 
       [horiz-margin 180]
       [style '(single password)]))

;; Click this button for encrypting a message.
(new button%
     [parent maingroup-one]
     [label "Encryption"]
     [callback (lambda (function event)
                 (define input (send text-one get-value))
                 (define key (send encryption-key get-value))
                 (send text-one set-value input)
                 (send encryption-key set-value key)
                 (send text-one set-value (final-encryption input key)))])
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::: Decryption box :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Box frame for the application components.
(define maingroup-two (new group-box-panel%
                           [label "To decrypt a message, insert your text here:"]
                           [parent maingroup]
                           [vert-margin 20]
                           [horiz-margin 5]
                           [alignment '(left center)]))

;; Text Editor where to insert the text for encrypting and decrypting.
(define text-two (new text-field%
                      [parent maingroup-two]
                      [label ""]
                      [init-value ""]
                      [style '(multiple)]))

;; Add a text field to the dialog
(define decryption-key
  (new text-field%
       [parent maingroup-two]
       [label "Secret Key"]
       [horiz-margin 180]
       [style '(single password)]))

;; Click this button for encrypting a message.
(new button%
     [parent maingroup-two]
     [label "Decryption"]
     [callback (lambda (function event) ; ASK: do we still need the args if unused?
                 (define input (send text-two get-value))
                 (define key (send decryption-key get-value))
                 (send text-two set-value input)
                 (send decryption-key set-value key)
                 (send text-two set-value (final-decryption input key)))])
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::: Documentation ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; Documentation window.
(define documentation (new frame%
                           [label "About"]
                           [width 600]
                           [height 600]))

; Box panel container for the message.
(define doc-box (new group-box-panel%
                     [parent documentation]
                     [label "How the algorithm works"]
                     [horiz-margin 30]
                     [vert-margin 30]))

; Box panel container for the message.
(define text-box (new group-box-panel%
                     [parent doc-box]
                     [label "Attention!"]
                     [horiz-margin 30]
                     [vert-margin 30]))

; Message showing on the documentation.
(define doc-text (new message%
                          [parent text-box]
                          [label "To send messages via internet, copy the encrypted message on a texteditor
 and zip it, then you can send the compressed file for decryption."]
                          [vert-margin 30]))

; Logo image on the welcome page.
(define doc-img (make-object bitmap%
               "map.png"
               'png/alpha))

; Message container for the image.
(define documentation-logo (new message%
                          [parent doc-box]
                          [label doc-img]))
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::: The end ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; ---> Start the second window of the Application.
;(send frame show #t)
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::