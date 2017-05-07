#lang racket

(require "profile.rkt")

; overall strategy: 
; we need to feed 3 blocks into the decrypt-profile function:
; block one needs to decrypt to "email=xxxxxxxxxx"
; block two should decrypt to "xxx&uid=10&role="
; block three should contain: "admin", with 11 bytes of pkcs7 padding 

; to create the first two blocks, pass in "xxxxxxxxxxxxx" to the encrypted-profile-for function, and keep the first two blocks that come back
(define ct (encrypted-profile-for "ice@gmail.com"))
(define subct (subbytes ct 0 32))

(printf "registered a profile for ice@gmail.com: \n")
ct
(printf "decrypted and parsed: ~a\n\n" (decrypt-profile ct))
; generate a string that will contain "admin" with pkcs padding to 16 bytes in the second encrypted block
(define admin (bytes->string/utf-8 (bytes-append #"admin" (make-bytes 11 11))))
(display "generating desired second-block string: ")
admin

(define firstblock "xxxxxxxxx@")
(set! admin (string-append firstblock admin))
(define ct2 (encrypted-profile-for admin))
(printf "generating profile for ~s:\n" admin)
ct2
(printf "decrypted and parsed: ~a\n\n" (decrypt-profile ct2))
; encrypt this, save the second block to use as the thirdblock in the next step
(define thirdblock (subbytes ct2 16 32))

; concatenate the first two blocks with the "special" third one
(define faked-ct (bytes-append subct thirdblock))
(display "decrypting this faked cyphertext: ")
faked-ct
(printf "profile created: ~a\n" (decrypt-profile faked-ct))
