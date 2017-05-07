#lang racket

(require net/url)
(require racket/tcp)
(require "../1/base64.rkt")

(define srv "/servlets/standalone.rkt?file=foo&signature=")

(define-values (cin cout) (tcp-connect "localhost" 8000))

(define current-html #"")

(define (padout bs)
  (bytes-append bs (make-bytes (- 20 (bytes-length bs)) 0)))

(define (time-to-fetch-from-server path)
  (with-handlers ([exn:fail? (lambda (exn)
                                        ;(displayln (exn-message exn))
                               ;(displayln "[reconnecting]")
                               (define-values (c1 c2) (tcp-connect "localhost" 8000))
                               (sleep .1)
                               (set! cin c1)
                               (set! cout c2)
                               (time-to-fetch-from-server path))])
    (let ([t0 (current-inexact-milliseconds)]
          [buffer (make-bytes 1024 0)])
      (fprintf cout "GET ~a HTTP/1.1\r\n\n" path)
      (flush-output cout)
      (set! current-html (subbytes buffer 0 (read-bytes-avail! buffer cin)))
      (- (current-inexact-milliseconds) t0))))

(define (get-next-byte sig)
    (let* ([buffer (make-bytes 1024 0)]
           [times (for/list ([i (in-range 256)])
                    (for/list ([j (in-range (cond [(> (bytes-length sig) 17) 5]
                                                  [(> (bytes-length sig) 10) 3]
                                                  [else 2]))])
                      (time-to-fetch-from-server (string-append srv (bytes->hex (padout (bytes-append sig (bytes i))))))))]
           [mins (for/list [(x times)]
                   (argmin (lambda (a) a) x))]
           [v (argmax (lambda (x) x) mins)])
      (bytes (- 256 (length (member v mins))))))

(define (fetch-html signature)
  (let ([buffer (make-bytes 1024 0)])
    (fprintf cout "GET ~a HTTP/1.1\r\n\n" (string-append srv signature))
    (flush-output cout)
    (subbytes buffer 0 (read-bytes-avail! buffer cin))))

(set! current-html (fetch-html "initial fetch"))
(printf "initial HTML: ~s~%" current-html)

(if (regexp-match #rx"200 Ok" (bytes->string/utf-8 current-html))
    (printf "Signature valid (not expected!)~%")
    (printf "Signature not valid (as expected)~%"))

(let ([sig #""])
  (for ([i (in-range 20)])
    (set! sig (bytes-append sig (get-next-byte sig)))
    (printf "so far: ~s~%" (bytes->hex sig)))
  (printf "final signature: ~a~%" (bytes->hex sig))
  (let ([final (fetch-html (bytes->hex sig))])
    (printf "final HTML: ~s~%" final)
    (if (regexp-match #rx"200 Ok" (bytes->string/utf-8 final))
        (printf "**pass~%")
        (printf "**fail~%"))))


