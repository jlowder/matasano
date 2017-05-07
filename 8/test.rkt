#lang racket

; iterate over each line of hex encoded data. decode and partition into 128-bit (32-char) chunks, as list. then remove duplicates and look for a list that is shorter than expected.

(define expected-blocks 10)

(define (test-line text)
  (let ([str (string-copy text)])
    (when (< (length (remove-duplicates (for/list ([i (in-range 0 (string-length text) 32)])
                                          (let ([j (substring str 0 32)])
                                            (set! str (substring str 32))
                                            j))))
             expected-blocks)
      (printf "this line appears to be ECB encoded: ~a\n" text))))


(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (test-line line)
      (read-next-line-iter file))))

(call-with-input-file "gistfile1.txt" read-next-line-iter)


