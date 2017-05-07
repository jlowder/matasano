#lang racket

(require "mt19937.rkt")

(initialize-generator 1)
(display "seeding generator with value 1\n")
(display "first 10 PRNs:\n")
(for ([i (in-range 10)])
     (display (extract-number))
     (display "\n"))
(display "
This matches the values from other implementations, e.g. https://github.com/cslarsen/mersenne-twister/blob/master/test-mt.cpp
and the C version converted from the wiki's pseudocode (see mt.c).\n")
