#lang racket 

(require "ctr.rkt")
(require "../1/base64.rkt")

(define input (base64->bytes "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="))

(define nonce (make-bytes 8 0))

(define key #"YELLOW SUBMARINE")

(printf "decrypting input data: ~s\n" (bytes->string/utf-8 (decrypt-aes-128-ctr nonce key input) #\.))

(define test-strings '(#"The 500 Hats of Bartholomew Cubbins"
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

(printf "\nmiscellaneous strings encrypted / decrypted with nonce = ~s and key = ~s\n" nonce key)

(for ([x (for/list ([x test-strings])
              (let ([ct (encrypt-aes-128-ctr nonce key x)])
                (printf "encrypting: ~s -> ~s\n" x ct)
                ct))])
  (printf "decrypting ~s -> ~s\n" x (decrypt-aes-128-ctr nonce key x)))

(set! nonce (bytes 1 2 3 4 5 6 7 8))
(set! key #"PILOT DIMPLES 82")

(printf "\nmiscellaneous strings encrypted / decrypted with nonce = ~s and key = ~s\n" nonce key)
(for ([x (for/list ([x test-strings])
              (let ([ct (encrypt-aes-128-ctr nonce key x)])
                (printf "encrypting: ~s -> ~s\n" x ct)
                ct))])
  (printf "decrypting ~s -> ~s\n" x (decrypt-aes-128-ctr nonce key x)))

