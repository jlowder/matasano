#lang racket

(provide score
         find-best-xor)

(require "../1/base64.rkt")
(require "../2/xor.rkt")

; information taken from http://en.wikipedia.org/wiki/Letter_frequency
(define eng (make-hash))
(hash-set! eng #\a .08167)
(hash-set! eng #\b .01492)
(hash-set! eng #\c .02782)
(hash-set! eng #\d .04253)
(hash-set! eng #\e .12702)
(hash-set! eng #\f .02228)
(hash-set! eng #\g .02015)
(hash-set! eng #\h .06094)
(hash-set! eng #\i .06966)
(hash-set! eng #\j .00153)
(hash-set! eng #\k .00772)
(hash-set! eng #\l .04025)
(hash-set! eng #\m .02406)
(hash-set! eng #\n .06749)
(hash-set! eng #\o .07507)
(hash-set! eng #\p .01929)
(hash-set! eng #\q .00095)
(hash-set! eng #\r .05987)
(hash-set! eng #\s .06327)
(hash-set! eng #\t .09056)
(hash-set! eng #\u .02758)
(hash-set! eng #\v .00978)
(hash-set! eng #\w .02360)
(hash-set! eng #\x .00150)
(hash-set! eng #\y .01974)
(hash-set! eng #\z .00074)

(define (count thing list)
  (cond [(null? list) 0]
        [else (if (equal? (first list) thing)
                  (+ 1 (count thing (rest list)))
                  (count thing (rest list)))]))

(define (make-freq)
  (define ret (make-hash))
  (for [(i (in-range (char->integer #\a) (+ 1 (char->integer #\z))))]
       (hash-set! ret (integer->char i) 0.0))
  ret)

(define (weigh text char)
  (/ (count char (string->list text)) (string-length text)))

(define (diffmap map1 map2)
  ; iterate over the keys of map1, summing the difference from map2
  (foldl + 0 (for/list [(i (hash-keys map1))]
                       (abs (- (hash-ref map1 i) (hash-ref map2 i))))))

(define (score text) ; compare the frequencies of text against the dictionary, produce a score which is the sum of differences. a perfect score is 0.
  (define basic (string-replace (string-downcase text) " " "")) ; remove all spaces and make downcase
  (define res (make-freq))
  (for [(i (in-range (char->integer #\a) (+ 1 (char->integer #\z))))]
       (hash-set! res (integer->char i) (weigh text (integer->char i))))
  (diffmap res eng))

(define (find-best-xor text) ; check all chars for the best looking xor key. return (key-byte best-score out-text in-text)
  (define best 100.0)
  (define best-byte 0)
  (define candidate 1.0)
  (for [(i 255)]
       (set! candidate (score (bytes->string/utf-8 (xor-bytes-int (hex->bytes text) i) #\.)))
       (when (< candidate best) ; lower is better
         (set! best candidate)
         (set! best-byte i)))
  (list best-byte best (bytes->string/utf-8 (xor-bytes-int (hex->bytes text) best-byte) #\.) text))
