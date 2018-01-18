#lang racket

(require redex "../src/Typecheck.rkt" rackunit)


(test-equal
 (term (multiplication ((1) (2 3)) () ))
 '((1) (2 3)))
(test-equal
 (term (multiplication () ((1) (2 3)) ))
 '((1) (2 3)))
(test-equal
 (term (multiplication ((1) (2 3)) ((8 2)) ))
 '((1 8 2) (2 3 8 2)))
(test-equal
 (term (multiplication ((1) (2 3)) ((8) (9)) ))
 '((1 8) (1 9) (2 3 8) (2 3 9)))

(println "All tests run successfully.")