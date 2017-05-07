#lang racket

; strategy: this is like #12, but everything has to shift down to the
; block after the prefix.  when determining the block size, the final
; block will contain only padding, which we save off. Then, add pad
; bytes until the encypted fill block reappears somewhere other than
; the last block. The number of pad bytes we had to insert tells us
; how many bytes short of a block boundary the prefix is (n -
; blocksize), and we can basically then repeat #12, prefixing
; everything with that many junk bytes.

(require "oracle.rkt")

; determine block size
(define pad-block #"") ; gets reset in the next statement

(define block-size
  ; keep trying longer strings until a jump in the output size is detected.
  (let ([j (bytes-length (encryption_oracle #"0"))])
    (for/or ([i (in-range 1 256)])
      (let* ([enc (encryption_oracle (make-bytes i 1))]
             [k (- (bytes-length enc) j)])
        (if (equal? 0 k)
             #f
             (begin (set! pad-block (subbytes enc (- (bytes-length enc) k))) k))))))

(printf "block size appears to be ~a bytes\n" block-size)
(printf "a block of encrypted padding looks like this: ~s\n" pad-block)

; confirm ecb mode
(if (equal? (encryption_oracle (make-bytes block-size 1))
            (encryption_oracle (make-bytes block-size 1)))
    (printf "it appears to be ECB\n")
    (printf "it appears to be non-ECB\n"))

(define (all-but-last list)
  (reverse (cdr (reverse list))))

(define (blockify bytes size) ; convert bytes into a list of subbytes, each of size size
  (cond [(>= (bytes-length bytes) size) (cons (list->bytes (take (bytes->list bytes) size)) 
                                              (blockify (subbytes bytes size) size))]
        [else empty]))

(define (position-of thing list)
  (for/or ([i (in-range 0 (length list))])
          (if (equal? thing (list-ref list i))
              i
              #f)))
         
; determine size of prefix
(define prefix-size
  (for/or ([i (in-range block-size (* block-size 2))])
          (let ([blocks (blockify (encryption_oracle (make-bytes i block-size)) block-size)])
            (if (member pad-block (all-but-last blocks))
                                        ; we are done. just determine where the match occurred, and we know how long the prefix is.
                (+ -16 (* block-size (position-of pad-block blocks)) (- block-size (remainder i block-size)))
                #f))))
          
(printf "prefix size looks like ~a bytes\n" prefix-size)

(define delta (- block-size (remainder prefix-size block-size)))

(printf "delta prefix size is ~a\n" delta)

(define (pad-bytes-required blocksize so-far)
  ; the target byte needs to be aligned in the last byte of the block
  ; it appears in. Return the number of left-side pad bytes necessary
  (let* ([target (bytes-length so-far)]
         [pad-bytes (+ delta (- (sub1 blocksize) (- target (* blocksize (quotient target blocksize)))))])
    pad-bytes))

(define (target-block blocksize so-far)
  ; index of the block that will have the result
  (quotient (+ (pad-bytes-required blocksize so-far) (bytes-length so-far) prefix-size) 16))

(define (pad-message blocksize so-far)
  ; the pad bytes appended with the message so-far
  (bytes-append (make-bytes (- (pad-bytes-required blocksize so-far) delta) 1) so-far))

(define (right-block blocksize so-far)
  ; return a byte string representing the one-byte-short block
  (let ([padded (pad-message blocksize so-far)])
    (subbytes padded (* blocksize (quotient (bytes-length padded) blocksize)))))
              
(define (make-dict blocksize so-far)
  ; build a dictionary that maps an encrypted final byte
  (let ([ht (make-hash)]
        [rb (right-block blocksize so-far)])
    (for ([i (in-range 256)])
         (let* ([res (encryption_oracle (bytes-append (make-bytes (+ delta (* blocksize (- (target-block blocksize so-far)
                                                                                           (target-block blocksize #"")))) 1)
                                                      rb
                                                      (bytes i)))]
             [subres (list-ref (blockify res blocksize) (target-block blocksize so-far))])
        (hash-set! ht subres i)))
    ht))

(define (find-next-byte blocksize dict so-far)
  (let* ([res (encryption_oracle (make-bytes (pad-bytes-required blocksize so-far) 1))]
         [subres (list-ref (blockify res blocksize) (target-block blocksize so-far))])
    (hash-ref! dict subres #f)))

; main loop
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
    
