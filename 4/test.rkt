#lang racket

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../3/freq.rkt")

(define inputfile "gistfile1.txt")

(define winner '())
(define best-score 100.0)

(define (find-best-xor text)
  (define best 100.0)
  (define best-byte 0)
  (define candidate 1.0)
  (for [(i 255)]
       (set! candidate (score (bytes->string/utf-8 (xor-bytes-int (hex->bytes text) i) #\.)))
       (when (< candidate best) ; lower is better
         (set! best candidate)
         (set! best-byte i)))
  (list best-byte best (bytes->string/utf-8 (xor-bytes-int (hex->bytes text) best-byte) #\.) text))
  
;(find-best-xor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

; iterate over all strings in the file
; for each, call find-best-xor and keep track of the best scores
; find the lowest score of everybody

(define (test-score text)
  (define candidate (find-best-xor text))
  (when (< (second candidate) best-score)
    (set! best-score (second candidate))
    (set! winner candidate)))

(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (test-score line)
      (read-next-line-iter file))))

(call-with-input-file inputfile read-next-line-iter)

(printf "~a appears to be the winner when xor'd with ~a (ascii \"~a\") to produce:\n"
        (fourth winner)
        (first winner)
        (integer->char (first winner)))
(third winner)
(printf "score was ~a\n" (second winner))
                       
