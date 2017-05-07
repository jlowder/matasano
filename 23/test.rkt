#lang racket

(require "distemper.rkt")
(require "spliced.rkt")
(require "../21/mt19937.rkt")

(display "testing temper/distemper functions:\n\n")

(for ([i (in-range 10)])
     (let* ([val (random 4294967087)]
            [t (temper val)]
            [i (distemper t)])
       (printf "tempering ~a: ~a\n" val t)
       (printf "inverting ~a: ~a\n" t i)
       (printf "**~a\n\n" (if (equal? val i)
                            "pass"
                            "fail"))))
(let ([seed (current-seconds)])
  (printf "initializing PRNG with ~a\n" seed)
  (initialize-generator seed))

; tap the prng 624 times, feed the distemperer

(display "tapping 624 values...\n")

(for ([i (in-range 624)])
     (let ([prn (extract-number)])
       (receive-sample prn)))

; extract the state, splice into another mt19937 instance
(display "splicing state into new generator...\n")

(store-state 0 (get-state))

; generate some more numbers from the OG prng and the predictor
(let ([og (for/list ([i (in-range 10)])
                    (extract-number))]
      [pr (for/list ([i (in-range 10)])
                   (predict-number))])
  (printf "Next 10 PRNs from the original PRNG:\n~a\n\n" og)
  (printf "Next 10 PRNs predicted by the spliced generator:\n~a\n\n" pr)
  (printf "**~a\n" (if (equal? og pr)
                       "pass"
                       "fail")))
  

       
