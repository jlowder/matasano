#lang racket

(require "oracle.rkt")
(require "../1/base64.rkt")
(require "../3/freq.rkt")
(require "../2/xor.rkt")

(define pts '("SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ=="
              "Q29taW5nIHdpdGggdml2aWQgZmFjZXM="
              "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ=="
              "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4="
              "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk"
              "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
              "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ="
              "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
              "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU="
              "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl"
              "VG8gcGxlYXNlIGEgY29tcGFuaW9u"
              "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA=="
              "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk="
              "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg=="
              "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo="
              "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
              "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA=="
              "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA=="
              "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA=="
              "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg=="
              "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw=="
              "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA=="
              "U2hlIHJvZGUgdG8gaGFycmllcnM/"
              "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w="
              "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4="
              "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ="
              "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs="
              "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA=="
              "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA=="
              "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4="
              "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA=="
              "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu"
              "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc="
              "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs"
              "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs="
              "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0"
              "SW4gdGhlIGNhc3VhbCBjb21lZHk7"
              "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw="
              "VHJhbnNmb3JtZWQgdXR0ZXJseTo="
              "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="))

(define cts (for/list ([x pts])
                      (ctr-encrypt (base64->bytes x))))

(define (process-position pos cts)
                                        ; extract char pos from each string in pts, splice them into one string and determine score
  (let* ([scores (for/list ([i (in-range 0 256)]) ; pick the lowest score of all values, meaning best fit for the english language
                           (score (list->string (for/list ([x cts]
                                                           #:when (> (bytes-length x) pos))
                                                          (integer->char (bitwise-xor i (bytes-ref x pos)))))))]
         [best (argmin (lambda (x) x) scores)])
    (for/or ([i (in-range 0 256)]
             #:when (equal? best (list-ref scores i)))
            i)))

(define derived-key (list->bytes (for/list ([i 38])
                                           (process-position i cts))))

(printf "decrypting using english language histogram:\n")

(for ([i (for/list ([x cts])
          (cond [(> (bytes-length x) (bytes-length derived-key)) (xor-bytes (subbytes x 0 (bytes-length derived-key)) derived-key)]
                [else (xor-bytes x (subbytes derived-key 0 (bytes-length x)))]))])
     (printf "~a\n" (bytes->string/utf-8 i #\.)))

(printf "\nSome characters towards the end of long sentences are garbled since\nthere were not enough characters in these positions for this type of analysis.\n
")

(printf "Further analysis could decipher those final characters, but enough is\nvisible now to see that this is from Easter, 1916 by William Butler Yeats:\n\n")

(printf "From http://www.poetryfoundation.org/poem/172061:\n\n")
(printf "I have met them at close of day\n")
(printf "Coming with vivid faces\n")
(printf "From counter or desk among grey\n")
(printf "Eighteenth-century houses.\n")
(printf "I have passed with a nod of the head\n")
(printf "Or polite meaningless words,\n")
(printf "Or have lingered awhile and said\n")
(printf "Polite meaningless words,\n")
(printf "And thought before I had done\n")
(printf "Of a mocking tale or a gibe\n")
(printf "To please a companion\n")
(printf "Around the fire at the club,\n")
(printf "Being certain that they and I\n")
(printf "But lived where motley is worn:\n")
(printf "All changed, changed utterly:\n")
(printf "A terrible beauty is born.\n")

(printf "That woman's days were spent\n")
(printf "In ignorant good-will,\n")
(printf "Her nights in argument\n")
(printf "Until her voice grew shrill.\n")
(printf "What voice more sweet than hers\n")
(printf "When, young and beautiful,\n")
(printf "She rode to harriers?\n")
(printf "This man had kept a school\n")
(printf "And rode our winged horse;\n")
(printf "This other his helper and friend\n")
(printf "Was coming into his force;\n")
(printf "He might have won fame in the end,\n")
(printf "So sensitive his nature seemed,\n")
(printf "So daring and sweet his thought.\n")
(printf "This other man I had dreamed\n")
(printf "A drunken, vainglorious lout.\n")
(printf "He had done most bitter wrong\n")
(printf "To some who are near my heart,\n")
(printf "Yet I number him in the song;\n")
(printf "He, too, has resigned his part\n")
(printf "In the casual comedy;\n")
(printf "He, too, has been changed in his turn,\n")
(printf "Transformed utterly:\n")
(printf "A terrible beauty is born.\n")
