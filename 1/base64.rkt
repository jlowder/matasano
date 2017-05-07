#lang racket

(provide bytes->hex
         bytes->base64
         hex->bytes
         base64->bytes
         number->bits
         hex->bits
         base64file->bytes
         hexfile->bytes)

(require data/bit-vector)

(define (number->bit-vector num)
  (string->bit-vector (number->string num 2)))

(define (number->bits num) ; list of #t and #f
  (bit-vector->list (number->bit-vector num)))

(define (hex-digit->bv digit)
  (define base (number->bits (string->number (string digit) 16)))
  (define pad (make-list (- 4 (length base)) #f))
  (list->bit-vector (append pad base)))

(define (hex->bits instr)
  (append* (map bit-vector->list (map hex-digit->bv (string->list instr)))))

(define (bits->number list)
  (foldl (lambda (fb sb) (+ (* 2 sb) (if fb 1 0))) 0 list))

(define (map64 val)
  (cond
   [(and (>= val 0)  (< val 26)) (integer->char (+ 65 val))]
   [(and (>= val 26) (< val 52)) (integer->char (+ 97 (- val 26)))]
   [(and (>= val 52) (< val 62)) (integer->char (+ 48 (- val 52)))]
   [(equal? val 62) #\+]
   [(equal? val 63) #\/]
   [#t #\=]))
         
(define (my-take list num)
  (when (< (length list) num)
      ; extend the list with #f's
      (set! list (append list (make-list (- num (length list)) #f))))
  (take list num))

(define (my-drop list num)
  (when (< (length list) num)
    (set! list (my-take list num)))
  (drop list num))

(define (bits->base64 list)
  ; iterate 6 bits at a time
  ; if fewer than 6 bits left, fill with zeros
  (define outs (open-output-string))
  (for ([i (ceiling (/ (length list) 6))])
    (write-string (string (map64 (bits->number (my-take list 6)))) outs)
    (set! list (my-drop list 6)))
  (get-output-string outs))

(define (expand-6 val)
  (define base (number->bits val))
  (if (equal? val 64)
      '()
      (append (make-list (- 6 (length base)) #f) base)))

(define (b64->bits chr)
  (expand-6 (cond
             [(and (char>=? chr #\A) (char<=? chr #\Z)) (- (char->integer chr) (char->integer #\A))]
             [(and (char>=? chr #\a) (char<=? chr #\z)) (+ 26 (- (char->integer chr) (char->integer #\a)))]
             [(and (char>=? chr #\0) (char<=? chr #\9)) (+ 52 (- (char->integer chr) (char->integer #\0)))]
             [(equal? chr #\+) 62]
             [(equal? chr #\/) 63]
             [(equal? chr #\=) 0])))

(define (padleft str)
  (if (even? (string-length str))
      str
      (string-append "0" str)))

(define (bytes->hex bytes)
  (define outs (open-output-string))
  (for [(i (in-range 0 (bytes-length bytes)))]
    (write-string (padleft (number->string (bytes-ref bytes i) 16)) outs))
    (get-output-string outs))

(define (bytes->base64 bytes)
  (define padding (- 3 (remainder (bytes-length bytes) 3)))
  (when (equal? padding 3)
    (set! padding 0))
  (define padstr (make-string padding #\=))
  (string-append (bits->base64 (hex->bits (bytes->hex bytes))) padstr))
 
(define (hex->bytes hex)
                                        ; pad on the left if an odd number of digits
  (when (odd? (string-length hex))
    (set! hex (string-append "0" hex)))
  
  (define hlist '())
  
                                        ; iterate over each pair of digits
  (for [(i (in-range 0 (string-length hex) 2))]
    (set! hlist (cons (+ (* 16 (string->number (string (string-ref hex i)) 16))
                         (string->number (string (string-ref hex (+ 1 i))) 16))
                      hlist)))
  (list->bytes (reverse hlist)))


(define (base64->bytes base64)
  ; every character adds 6 bits of data
  ; = chars treated differently. (ironic?)
  (define hlist '())
  (define data '())
  (for [(i (string-length base64))]
    (set! data (append data (b64->bits (string-ref base64 i)))))
  
  ; determine the number of pad bytes to remove from the right
  (define pad-bytes (cond
                     [(regexp-match #rx"==" base64) 2]
                     [(regexp-match #rx"=" base64) 1]
                     [else 0]))
  
  ;(set! data (append (make-list padding #f) data))
  (for [(i (in-range 0 (length data) 8))]
    (set! hlist (cons (string->number (bit-vector->string (list->bit-vector (take data 8))) 2) hlist))
    (set! data (drop data 8)))
  (list->bytes (take (reverse hlist) (- (length hlist) pad-bytes))))

(define lines #"")

(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (set! lines (bytes-append lines (hex->bytes line)))
      (read-next-line-iter file))))

(define (read-next-line-iter64 file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (when (string? line)
        (set! lines (bytes-append lines (base64->bytes line))))
      (read-next-line-iter64 file))))

(define (hexfile->bytes inputfile)
  (set! lines #"")
  (call-with-input-file inputfile read-next-line-iter))

(define (base64file->bytes inputfile)
  (set! lines #"")
  (call-with-input-file inputfile read-next-line-iter64)
  lines)
