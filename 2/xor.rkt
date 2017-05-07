#lang racket

(provide xor-bytes
         xor-bytes-int
         xor-bytes-ints)

(define (xor-bytes b1 b2) ; xor two equal length byte strings
  (list->bytes (for/list [(i (in-range 0 (bytes-length b1)))]
                         (bitwise-xor (bytes-ref b1 i) (bytes-ref b2 i)))))

(define (xor-bytes-int b1 int) ; byte string xor'd with a single val
  (list->bytes (for/list [(i (in-range 0 (bytes-length b1)))]
                         (bitwise-xor (bytes-ref b1 i) int))))

(define (padout lst seq) ; make seq repeat until same length as lst
  (cond [(>= (length seq) (length lst)) (take seq (length lst))]
        [else (padout lst (append seq seq))]))

(define (xor-bytes-ints b1 b2) ; byte string xor'd with a repeating pattern
  (list->bytes (for/list [(i (bytes->list b1))
                          (j (padout (bytes->list b1) (bytes->list b2)))]
                         (bitwise-xor i j))))
 
