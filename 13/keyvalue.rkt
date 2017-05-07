#lang racket

(provide parse-cookie-string)

(require json)

(define (parse-cookie-string str) ; parse cookie string into json
  (let ([map (make-hash)])
    (for ([i ; split at ampersands, and then at semicolons
           (for/list ([i (regexp-split "&" str)])
             (regexp-split "=" i))])
      (hash-set! map (string->symbol (first i)) (second i)))
    (jsexpr->string map)))

