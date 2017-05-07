I am using the "insta" server that comes bundled with racket for
exercises 31 and 32. I had a really hard time getting 31 to complete
successfully since insta is very "jittery" for some reason - I think
it may have something to do with the way garbage collection is
implemented in the runtime environment. So, in exercise 31 I had to
make the app take multiple samples of the roundtrip request/response
times for each byte, and do it adaptively so that longer byte strings
take more samples, etc. But, once I finally got it working it seems
pretty solid; for exercise 32 I dropped the artificial delay down to
5ms and it still worked ok - it just ran a lot faster. I had to take
it down to 1 millisecond to make it fail:

initial HTML: #"HTTP/1.1 500 Okay\r\nDate: Thu, 05 Sep 2013 21:21:03 GMT\r\nLast-Modified: Thu, 05 Sep 2013 21:21:03 GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nTransfer-Encoding: chunked\r\n\r\n15c\r\n<html><head><title>HMAC checker</title></head><body><h1>HMAC Checker: FAIL</h1><div class=\"posts\"><div class=\"post\"><p>file: foo</p><p>expected signature: initial fetch</p><p>actual signature:   8913a06237dfe5c059f4693636dee7203ac422cc</p></div></div><form><input name=\"file\" /><input name=\"signature\" /><input type=\"submit\" /></form></body></html>\r\n0\r\nContent-Length: 348\r\n\r\n"
Signature not valid (as expected)
so far: "13"
so far: "139a"
so far: "139a1f"
so far: "139a1f7c"
so far: "139a1f7c59"
so far: "139a1f7c59a2"
so far: "139a1f7c59a2e0"
so far: "139a1f7c59a2e00b"
so far: "139a1f7c59a2e00bda"
so far: "139a1f7c59a2e00bdac0"
so far: "139a1f7c59a2e00bdac0d4"
so far: "139a1f7c59a2e00bdac0d42a"
so far: "139a1f7c59a2e00bdac0d42a9f"
so far: "139a1f7c59a2e00bdac0d42a9f1a"
so far: "139a1f7c59a2e00bdac0d42a9f1a90"
so far: "139a1f7c59a2e00bdac0d42a9f1a90cb"
so far: "139a1f7c59a2e00bdac0d42a9f1a90cb4f"
so far: "139a1f7c59a2e00bdac0d42a9f1a90cb4fc6"
so far: "139a1f7c59a2e00bdac0d42a9f1a90cb4fc6e4"
so far: "139a1f7c59a2e00bdac0d42a9f1a90cb4fc6e4df"
final signature: 139a1f7c59a2e00bdac0d42a9f1a90cb4fc6e4df
final HTML: #"HTTP/1.1 500 Okay\r\nDate: Thu, 05 Sep 2013 21:22:00 GMT\r\nLast-Modified: Thu, 05 Sep 2013 21:22:00 GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nTransfer-Encoding: chunked\r\n\r\n177\r\n<html><head><title>HMAC checker</title></head><body><h1>HMAC Checker: FAIL</h1><div class=\"posts\"><div class=\"post\"><p>file: foo</p><p>expected signature: 139a1f7c59a2e00bdac0d42a9f1a90cb4fc6e4df</p><p>actual signature:   8913a06237dfe5c059f4693636dee7203ac422cc</p></div></div><form><input name=\"file\" /><input name=\"signature\" /><input type=\"submit\" /></form></body></html>\r\n0\r\nContent-Length: 375\r\n\r\n"
**fail

I was able to make minimal tweaks to the test app to get it working again:

diff test.rkt ../31/test.rkt 
35,37c35,37
<                     (for/list ([j (in-range (cond [(> (bytes-length sig) 17) 8]
<                                                   [(> (bytes-length sig) 10) 5]
<                                                   [else 3]))])
---
>                     (for/list ([j (in-range (cond [(> (bytes-length sig) 17) 5]
>                                                   [(> (bytes-length sig) 10) 3]
>                                                   [else 2]))])

So, the overall algorithm didn't require any changes - just
adjustments to the number of samples it collects per byte.
