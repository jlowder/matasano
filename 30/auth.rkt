#lang racket

(require "md4.rkt")

(provide authenticate validate)

(define words "/usr/share/dict/words")

(define generator (make-pseudo-random-generator))

(define word-list '())

(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (set! word-list (cons line word-list))
      (read-next-line-iter file))))

(call-with-input-file words read-next-line-iter)

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (random-element list)
  (list-ref list (random (length list))))

(define random-key (string->bytes/utf-8 (list-ref word-list (random (length word-list) generator))))

(printf "secret key: ~a~%" random-key)

(define (authenticate message)
  (md4 (bytes-append random-key message)))

(define (validate mac message)
  (equal? (authenticate message) mac))

                      
