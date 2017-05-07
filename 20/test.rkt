#lang racket

(require "../1/base64.rkt")
(require "../3/freq.rkt")
(require "../2/xor.rkt")
(require "oracle.rkt")

(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (cond [(eof-object? line) '()]
          [else (cons (ctr-encrypt (base64->bytes line)) (read-next-line-iter file))])))

(define cts (call-with-input-file "gistfile1.txt" read-next-line-iter))

(define shortest-cts (argmin (lambda (x) x) (for/list ([i cts])
                       (bytes-length i))))

(printf "truncating cypher texts to ~a bytes (length of shortest one)\n\n" shortest-cts)

(define tcts (for/list ([i cts]) ; truncated cypher texts
               (subbytes i 0 shortest-cts)))

(define (process-position pos cts tweaks)
                                        ; extract char pos from each string in cts, splice them into one string and determine score
  (let* ([scores (for/list ([i (in-range 0 256)]) ; pick the lowest score of all values, meaning best fit for the english language
                           (score (list->string (for/list ([x cts]
                                                           #:when (> (bytes-length x) pos))
                                                          (integer->char (bitwise-xor i (bytes-ref x pos)))))))]
         [sorted (sort scores <)]
         [best (if (member pos tweaks) ; allow a list of tweaks to be passed in. A tweaked column means get the second best score rather than first best.
                   (second sorted)
                   (first sorted))])
    (for/or ([i (in-range 0 256)]
             #:when (equal? best (list-ref scores i)))
            i)))

(define derived-key (list->bytes (for/list ([i shortest-cts])
                                   (process-position i tcts '(0 28)))))

(for ([i (for/list ([x tcts])
           (xor-bytes x derived-key))])
  (printf "~a\n" (bytes->string/utf-8 i #\.)))
