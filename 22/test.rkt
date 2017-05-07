#lang racket

(require "prn.rkt")
(require "../21/mt19937.rkt")

(let ([prn (prn)]
      [tseed (current-seconds)])
  (printf "PRN: ~a\n" prn)
  (printf "looking backwards from ~a\n" tseed)
  (let ([candidate (for/or ([i (in-range tseed (- tseed 86400) -1)])
                           (initialize-generator i)
                           (if (equal? (extract-number) prn)
                               i
                               #f))])
    (printf "found: ~a\n" candidate)
    (printf "~a\n" (test-seed candidate))))
