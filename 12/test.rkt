#lang racket

(provide pad-bytes-required
         right-block
         target-block
         blockify
         make-dict)

(require "oracle.rkt")

; determine block size
(define block-size
  ; keep trying longer strings until a jump in the output size is detected.
  (let ([j (bytes-length (encryption_oracle #"0"))])
    (for/or ([i (in-range 1 256)])
      (let ([k (- (bytes-length (encryption_oracle (make-bytes i 1))) j)])
        (if (equal? 0 k)
             #f
             k)))))

(printf "block size appears to be ~a bytes\n" block-size)

(define (blockify bytes size) ; convert bytes into a list of subbytes, each of size size
  (cond [(>= (bytes-length bytes) size) (cons (list->bytes (take (bytes->list bytes) size)) 
                                              (blockify (subbytes bytes size) size))]
        [else empty]))

; confirm ecb mode
(if (equal? (encryption_oracle (make-bytes block-size 1))
            (encryption_oracle (make-bytes block-size 1)))
    (printf "it appears to be ECB\n")
    (printf "it appears to be non-ECB\n"))

(define (pad-bytes-required blocksize so-far)
  ; the target byte needs to be aligned in the last byte of the block
  ; it appears in. Return the number of left-side pad bytes necessary
  (let* ([target (bytes-length so-far)]
         [pad-bytes (- (sub1 blocksize) (- target (* blocksize (quotient target blocksize))))])
    pad-bytes))

(define (target-block blocksize so-far)
  ; index of the block that will have the result
  (quotient (+ (pad-bytes-required blocksize so-far) (bytes-length so-far)) 16))

(define (pad-message blocksize so-far)
  ; the pad bytes appended with the message so-far
  (bytes-append (make-bytes (pad-bytes-required blocksize so-far) 1) so-far))

(define (right-block blocksize so-far)
  ; return a byte string representing the one-byte-short block
  (let ([padded (pad-message blocksize so-far)])
    (subbytes padded (* blocksize (quotient (bytes-length padded) blocksize)))))
              
(define (make-dict blocksize so-far)
  ; build a dictionary that maps an encrypted final byte
  (let ([ht (make-hash)]
        [rb (right-block blocksize so-far)])
    (for ([i (in-range 256)])
      (let* ([res (encryption_oracle (bytes-append (make-bytes (* blocksize (target-block blocksize so-far)))
                                                   rb
                                                   (bytes i)))]
             [subres (list-ref (blockify res blocksize) (target-block blocksize so-far))])
        (hash-set! ht subres i)))
    ht))

(define (find-next-byte blocksize dict so-far)
  (let* ([res (encryption_oracle (make-bytes (pad-bytes-required blocksize so-far) 1))]
         [subres (list-ref (blockify res blocksize) (target-block blocksize so-far))])
    (hash-ref! dict subres #f)))

(let ([dict (make-dict block-size #"")]
      [output #""])
  (for ([i (in-range (bytes-length (encryption_oracle #"")))]) ; use length returned as max number of bytes
    (let ([next (find-next-byte block-size dict output)])
      (when (integer? next)
        (set! output (bytes-append output (bytes next)))
        (display output)
        (display "\n")
        (set! dict (make-dict block-size output)))))
  (display "\n\nDecoded byte string:\n")
  output)
    
