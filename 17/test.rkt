#lang racket

(require "oracle.rkt")
(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../15/unpad.rkt")

(define (decrypt-byte block iv pos target suffix) ; decrypt a single byte within a block
  ;(printf "looking at ~a ~a ~a ~a ~s\n" (bytes->hex block) (bytes->hex iv) pos target suffix)
  (let ([r 0]
        [p 0])
    (for/or ([i (in-range 0 256)])
      (let ([fake-iv (bytes-append (make-bytes pos 0) (bytes i) (list->bytes suffix))])
        (if (check-decrypted-padding fake-iv block)
            (let* ([result (xor-bytes (bytes target) (xor-bytes (bytes (bytes-ref iv pos)) (bytes i)))]
                   [pad (xor-bytes result (bytes (bytes-ref iv pos)))])
              (set! r (car (bytes->list result)))
              (set! p (car (bytes->list pad))))
            #f)))
    (values r p)))

(define (decrypt-block block iv) ; decrypt a single 16-byte block
  ;(printf "decrypting ~a ~a\n" (bytes->hex block) (bytes->hex iv))
  (let ([suffix '()])
    (reverse (for/list ([i (in-range 15 -1 -1)]
               [val (in-range 1 17)])
      (let-values ([(res pad) (decrypt-byte block iv i val (for/list ([j suffix])
                                                             (bitwise-xor j val)))])
        (set! suffix (cons pad suffix))
        res)))))
  
(define (decrypt-message bytes iv) ; decrypt a single message, potentially containing multiple blocks
  (let decrypt-blocks ([data bytes]
                       [iv iv])
    (cond [(< (bytes-length data) 16) '()]
          [else (cons (decrypt-block (subbytes data 0 16) iv) (decrypt-blocks (subbytes data 16) (subbytes data 0 16)))])))

(define (try-unpad bytes)
    (with-handlers ([exn:fail? (lambda (exn) #f)])
      (unpad bytes)
      #t))

(let ([strings '()])
  (for ([i (in-range 100)]) ; do 100 times, hopefully enough to get all possible responses
    (let-values ([(ct iv) (encryption-oracle)])  ; call oracle, save cyphertext and iv
      (let ([pt (list->bytes (flatten (decrypt-message ct iv)))]) ; attempt to decrypt
        (when (try-unpad pt) ; if there was an error decrypting it will not unpad. only save the ones that are successful.
          (set! strings (cons (bytes->string/utf-8 (unpad pt)) strings))))))
  (for ([s (sort (remove-duplicates strings) string<?)])
    (printf "~s\n" s)))


