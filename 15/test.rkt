#lang racket
(require "unpad.rkt")

(define (try-unpad x)
  (with-handlers ([exn:fail? (lambda (exn)
                               (displayln (exn-message exn))
                               #f)])
    (printf "attempting to unpad ~s\n" x)
    (unpad x)))


(printf "~s\n" (try-unpad #"ICE ICE BABY\4\4\4\4"))
(printf "~s\n" (try-unpad #"ICE ICE BABY\5\5\5\5"))
(printf "~s\n" (try-unpad #"ICE ICE BABY\1\2\3\4"))

