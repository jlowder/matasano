#lang racket

(require "../1/base64.rkt")

(provide find-best-key-size
         transpose-bytes
         hamming)

(define (hamming b1 b2) ; compute hamming distance between two byte strings
  (for/sum [(i (hex->bits (bytes->hex b1)))
            (j (hex->bits (bytes->hex b2)))]
           (cond
            [(equal? i j) 0]
            [else 1])))
         
(define blocks-to-average 9)
(define (find-best-key-size bytes) ; determine the most likely xor key size
  (define scores 
    (for/list ([keysize (in-range 2 40)])
              (/ (for/sum ([i (in-range keysize (* blocks-to-average keysize) keysize)])
                          (hamming (subbytes bytes 0 keysize) (subbytes bytes i (+ i keysize)))) (* blocks-to-average keysize))))
  ;(display (map real->decimal-string scores))
  (for/or ([i (length scores)]
           #:when (equal? (list-ref scores i) (argmin (lambda (z) z) scores)))
          (+ 2 i)))

(define (transpose-bytes bytes num) ; partition bytes into num blocks, round robin
  (for/list ([i num])
            (list->bytes (for/list ([j (in-range i (bytes-length bytes) num)])
                      (bytes-ref bytes j)))))

