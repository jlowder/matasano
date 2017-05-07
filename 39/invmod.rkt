#lang racket

(require math/number-theory)

(provide invmod)

(define (invmod1 a b x0 x1)
  (if (> a 1)
      (let ((q (quotient a b))
            (t b)
            (t1 x0))
        (set! b (remainder a b))
        (set! a t)
        (set! x0 (- x1 (* q x0)))
        (set! x1 t1)
        (invmod1 a b x0 x1))
      x1))

(define (invmod a b)
  (if (coprime? a b)
      (let ((b0 b))
        (if (equal? b 1)
            1
            (let ((x1 (invmod1 a b 0 1)))
              (when (< x1 0)
                (set! x1 (+ b0 x1)))
              x1)))
      (begin
        (printf "Error -- ~a and ~a are not co-prime\n" a b)
        0)))
  
