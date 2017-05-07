#lang racket

(provide gen-pad pad-of)
(require "../1/base64.rkt")
(require "auth.rkt")
(require "sha1.rkt")

(define (gen-pad n)
  (let ([p (- 63 (modulo (+ n 8) 64))])
    (bytes-append (bytes 128) (make-bytes p 0))))
        
(define (pad-of n [keysize 8])
  (bytes-append (gen-pad (+ (bytes-length n) keysize)) (integer->integer-bytes (* 8 (+ keysize (bytes-length n))) 8 #f #t)))

(define original-message #"comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon")

(define signature (authenticate original-message))
(define new-message #";admin=true")

(printf "Using message ~s~%" original-message)

(printf "Message length: ~a bytes~%" (bytes-length original-message))

(printf "Authenticated signature is ~a~%" (bytes->hex signature))
(printf "signature is ~a~%" (cond [(validate signature original-message) "valid"]
                                  [else "invalid"]))
(printf "new message is ~s~%" new-message)
(for/or ([i (in-range 1 100)])
  (let* ([glue-padding (pad-of original-message i)]
         [forged-sig (sha1 new-message
                           (integer-bytes->integer signature #f #t 0 4)
                           (integer-bytes->integer signature #f #t 4 8)
                           (integer-bytes->integer signature #f #t 8 12)
                           (integer-bytes->integer signature #f #t 12 16)
                           (integer-bytes->integer signature #f #t 16 20)
                           (* 8 (+ i (bytes-length original-message) (bytes-length glue-padding) (bytes-length new-message))))])
    (if (validate forged-sig (bytes-append original-message glue-padding new-message))
        (begin
          (printf "full message is: ~s~%" (bytes-append original-message glue-padding new-message))
          (printf "the valid MD is: ~s~%" (bytes->hex forged-sig))
          (printf "with key length of ~a bytes and glue pad of ~s~%" i (bytes->hex glue-padding)))
        #f)))


