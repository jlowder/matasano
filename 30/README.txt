I think racket is too new/obscure of language to have native MD4
implementations laying around on the internet - I spent a lot of time
looking and ultimately came up empty. (I even had trouble finding a
"good" sha1 implementation since the preferred method of doing sha1 in
racket is to use FFI to call openssl directly; the first native racket
sha1 implementation I found was subtly flawed, and I wasted hours on
it). So, at first I was going to write my own sha1 module in racket by
porting a C implemtation. But then I realized this might be a good
opportunity to learn about racket's FFI mechanism, so I decided to
stick with the C implementation. I recompiled the C library as a
shared object library:

gcc -fpic -c md4.c
gcc -shared -o md4.so

Then, I wrote md4.rkt to interface with this library through FFI. I
also created md4-test-vectors.rkt to validate the results. (If I had
done this with the sha1 exercise, it would have saved me a lot of time
in the long run :)

The C code is from: ftp://ftp.caughq.org/pub/src/crypto/md4.c
I made some minor changes to allow the state to be passed in.
