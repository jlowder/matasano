#lang racket

(define (gen-answers dir)
  (printf "Answers for set~a:\n" (add1 (quotient dir 8)))
  (for ([i (in-range dir (+ 8 dir))])
    (printf "\n")
    (printf "// ------------------------------------------------------------\n")
    (printf "\n")
    (printf "~a. Output of running racket on \"~a/test.rkt\":\n\n" i i)
      (let ([sp (process (format "cat ~a/answer.txt" i))])
        (for ([line (in-lines (first sp))])
          (display line)
          (printf "\n")))))
  
(gen-answers (string->number (vector-ref (current-command-line-arguments) 0)))
