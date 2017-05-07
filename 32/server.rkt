;#lang web-server/insta
#lang racket

(require "hmac.rkt")
(require "../1/base64.rkt")
(require web-server/servlet
         web-server/servlet-env)

(struct post (file signature))

(define PAIR '())

; start: request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (local [(define a-pair
            (cond [(can-parse-post? (request-bindings request))
                   (cons (parse-post (request-bindings request))
                         PAIR)]
                  [else
                   PAIR]))]
    (render-page a-pair request)))


; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-post? bindings)
  (and (exists-binding? 'file bindings)
       (exists-binding? 'signature bindings)))

; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-post bindings)
  (post (extract-binding/single 'file bindings)
        (extract-binding/single 'signature bindings)))

(define (insecure-compare bs str)
  (not (for/or ([x (bytes->hex bs)]
                [y str])
         (if (equal? x y)
             (begin
               (sleep 1/1000)
               #f)
             #t))))

; render-page: pair request -> response Consumes a pair and a request,
; and produces an HTML page (200) if valid, or error (500) if invalid
(define (render-page a-pair request)
  ; if no data passed via the request, render a plain page. otherwise render according to results.
  (if (empty? a-pair)
      (response/xexpr
       `(html (head (title "HMAC checker"))
              (body
               (h1 "HMAC Checker")
               ,(render-posts a-pair)
               (form
                (input ((name "file")))
                (input ((name "signature")))
                (input ((type "submit")))))))
                                        ; determine if the sig is ok. if good, render the page, otherwise return a 500.
      (if (insecure-compare (hmac-file (string-append "/home/jlowder/SpiderOak Hive/matasano/31/" (post-file (first a-pair))))
                            (post-signature (first a-pair)))
          (response/xexpr
           `(html (head (title "HMAC checker"))
                  (body
                   (h1 "HMAC Checker: SUCCESS")
                   ,(render-posts a-pair)
                   (form
                    (input ((name "file")))
                    (input ((name "signature")))
                    (input ((type "submit")))))))
          (response/xexpr
           #:code 500
           `(html (head (title "HMAC checker"))
                  (body
                   (h1 "HMAC Checker: FAIL")
                   ,(render-posts a-pair)
                   (form
                    (input ((name "file")))
                    (input ((name "signature")))
                    (input ((type "submit"))))))))))


; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-post a-post)
  `(div ((class "post"))
        (p ,(~a "file: " (post-file a-post)))
        (p ,(~a "expected signature: " (post-signature a-post)))
        (p ,(~a "actual signature:   " (bytes->hex (hmac-file (string-append "/home/jlowder/SpiderOak Hive/matasano/31/" (post-file a-post))))))))

; render-posts: pair -> xexpr
; Consumes a pair, produces an xexpr fragment
; of all its posts.
(define (render-posts a-pair)
  `(div ((class "posts"))
        ,@(map render-post a-pair)))

(display "url: http://localhost:8000/servlets/standalone.rkt\n")
(serve/servlet start #:port 8000  #:command-line? #t)
