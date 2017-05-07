#lang racket/base

(provide hmac-bytes hmac-file)

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../28/sha1.rkt")

(define words "/usr/share/dict/words")

(define generator (make-pseudo-random-generator))

(define word-list '())

(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (set! word-list (cons line word-list))
      (read-next-line-iter file))))

(call-with-input-file words read-next-line-iter)

(define (random-element list)
  (list-ref list (random (length list))))

(define random-key (string->bytes/utf-8 (list-ref word-list (random (length word-list) generator))))

(printf "secret key: ~a~%" random-key)

(define (hmac-file file [key random-key])
  (let* ([fp (open-input-file file #:mode 'binary)]
         [contents (read-bytes 1024000 fp)])
    (close-input-port fp)
    (hmac-bytes contents key)))

(define (hmac-bytes message [key random-key])
  (let* ([padded-key (bytes-append key (make-bytes (- 64 (bytes-length key)) 0))]
         [opad (xor-bytes (make-bytes 64 #x5C) padded-key)]
         [ipad (xor-bytes (make-bytes 64 #x36) padded-key)])
    (sha1 (bytes-append opad (sha1 (bytes-append ipad message))))))

; from wikipedia
;function hmac (key, message)
;    if (length(key) > blocksize) then
;        key = hash(key) // keys longer than blocksize are shortened
;    end if
;    if (length(key) < blocksize) then
;        key = key ∥ [0x00 * (blocksize - length(key))] // keys shorter than blocksize are zero-padded ('∥' is concatenation) 
;    end if
;   
;    o_key_pad = [0x5c * blocksize] ⊕ key // Where blocksize is that of the underlying hash function
;    i_key_pad = [0x36 * blocksize] ⊕ key // Where ⊕ is exclusive or (XOR)
;   
;    return hash(o_key_pad ∥ hash(i_key_pad ∥ message)) // Where '∥' is concatenation
;end function
