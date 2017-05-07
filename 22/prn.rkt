#lang racket

(provide prn test-seed)

(require "../21/mt19937.rkt")

(define (random-wait)
  (sleep (+ 40 (random 960))))

(define secret-seed 0)

(define (prn)
  (random-wait)
  (set! secret-seed (current-seconds))
  (initialize-generator secret-seed)
  (random-wait)
  (extract-number))

(define (test-seed seed)
  (if (equal? seed secret-seed)
      "**pass"
      "**fail"))
