#lang racket

(require "sha1.rkt")
(require "auth.rkt")
(require "../1/base64.rkt")
(require "../2/xor.rkt")

(define message #"But you seem like clean and virtuous boys.")
(define signature (authenticate message))
(printf "Using message ~s~%" message)
(printf "Authenticated signature is ~a~%" (bytes->hex signature))
(printf "signature is ~a~%" (cond [(validate signature message) "valid"]
                                  [else "invalid"]))
(define mucked-sig (xor-bytes (bytes-append (make-bytes (- (bytes-length signature) 1) 0) (bytes 1)) signature))
(printf "changing one bit in the signature: ~a~%" (bytes->hex mucked-sig))
(printf "checking mucked signature with valid message: ~a~%" (cond [(validate mucked-sig message) "valid"]
                                                                   [else "invalid"]))
(define mucked-message (xor-bytes (bytes-append (make-bytes (- (bytes-length message) 1) 0) (bytes 1)) message))
(printf "changing one bit in the message: ~s~%" mucked-message)
(printf "checking valid signature against mucked message: ~a~%" (cond [(validate signature mucked-message) "valid"]
                                                                      [else "invalid"]))

(define mymac (sha1 message))
(printf "trying to fabricate a MAC: ~a~%" (bytes->hex mymac))
(printf "checking my faked MAC: ~a~%" (cond [(validate mymac message) "valid"]
                                            [else "invalid"]))
