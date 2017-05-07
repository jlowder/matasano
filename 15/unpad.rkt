#lang racket

(provide unpad)

(define (check-pad list val left)
  (cond [(not (equal? val (car list))) #f]
        [(equal? 1 left) #t]
        [else (check-pad (cdr list) val (sub1 left))]))

(define (unpad bytes)
  ; whatever the last byte of bytes is, there should be that many of
  ; those bytes at the end, atherwise throw an exception. Return the
  ; value with the padding removed.
  (let ([val (reverse (bytes->list bytes))]) ; reverse, since it's easier in lisp to work at the front.
    (unless (check-pad val (car val) (car val))
      (raise-user-error 'invalid-pad "The PKCS7 padding is not correct on ~s" bytes))
    (list->bytes (reverse (drop val (car val))))))

 
