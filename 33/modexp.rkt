#lang racket
(provide mod-exp)

(define (mod-exp base expn mod)
  (if (= expn 1)
      (modulo base mod)
      (letrec ([b (mod-exp base (quotient expn 2) mod)]
               [c (modulo (* b b) mod)])
        (if (odd? expn)
            (* c (modulo base mod))
            c))))
