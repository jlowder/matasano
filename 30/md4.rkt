#lang racket/base

(provide md4)

(require ffi/unsafe
         ffi/unsafe/define)
 
(define-ffi-definer define-md4 (ffi-lib "md4"))
(define-md4 mdfour (_fun _bytes _bytes _int _uint32 _uint32 _uint32 _uint32 _uint32 -> _void))
(define buffer (make-bytes 16 0))

(define (md4 ; wrapper the C library
         bs
         [a #x67452301]
         [b #xefcdab89]
         [c #x98badcfe]
         [d #x10325476]
         [len (* 8 (bytes-length bs))])
  (let ([buffer (make-bytes 16 0)])
    (mdfour buffer bs (bytes-length bs) a b c d len)
    buffer))

