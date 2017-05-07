#lang racket

(require "../1/base64.rkt")
(require "oracle.rkt")

(define questions '(#"The 500 Hats of Bartholomew Cubbins"
                    #"And To Think That I Saw It On Mulberry Street"
                    #"Bartholomew and the Oobleck"
                    #"The Butter Battle Book"
                    #"The Cat in the Hat Songbook"
                    #"Daisy-Head Mayzie"
                    #"Did I Ever Tell You How Lucky You Are?"
                    #"Dr. Seuss's Sleep Book"
                    #"Gerald McBoing Boing"
                    #"Horton Hatches the Egg"
                    #"Horton Hears A Who!"
                    #"How the Grinch Stole Christmas"
                    #"Hunches in Bunches"
                    #"I Can Lick 30 Tigers Today! and Other Stories"
                    #"I Had Trouble in Getting to Solla Sollew"
                    #"If I Ran the Circus"
                    #"If I Ran the Zoo"
                    #"The King's Stilts"
                    #"The Lorax"
                    #"McElligot's Pool"
                    #"My Book About Me"
                    #"Oh, the Places You'll Go!"
                    #"On Beyond Zebra!"
                    #"Scrambled Eggs Super!"
                    #"The Sneetches and Other Stories"
                    #"Thidwick the Big-Hearted Moose"
                    #"Yertle the Turtle and Other Stories"))

(for ([question questions])
  ; ask oracle for an answer
  (let-values ([(answer y) (encryption_oracle question)])
    (printf "asked the oracle:\n~s (hex ~s)\n" question (bytes->hex question))
    (printf "got back:\n~s\n" (bytes->hex answer))

    ; now determine if that was ecb or cbc. Send it back in a zillion
    ; times, see if we ever get a duplicate.
    (define it-was (if (for/or ([i (in-range 1000)]) ; this short-circuits as soon as it finds something equal (which means ECB was used)
                         (let-values ([(this-answer xx) (encryption_oracle question)]) 
                           (equal? this-answer answer)))
                       "ECB"
                       "CBC"))
    
    (printf "I determined ~a was used\n" it-was)
    (if (equal? it-was y)
        (printf "** pass\n")
        (printf "** fail\n")))
  (display "\n"))


