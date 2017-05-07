For this problem, I defined a "participant" class that is instantiated
for both A and B, and a "man-in-the-middle" class for M. In order to
simulate the network protocol, data is passed between objects using
"ports", which is an I/O abstraction that racket uses. This is fairly
realistic since in racket, sending and receiving data over a real
network would also be done with ports.
