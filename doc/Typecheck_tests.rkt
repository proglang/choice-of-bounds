#lang racket

(require redex "../src/Typecheck.rkt" rackunit)

; while

(define incrWhile
  (term (while ((num 1)) do {
    ((x := (x + (num -1))) then ((w := x) then (
                    (x := y) then (y := z))))})))

(test-equal
 (first (judgment-holds (▷ () ((w ()) (x ()) (y ()) (z ((42)))) () ,incrWhile : Γ) Γ))
 (term ((w ((42))) (x ((42))) (y ((42))) (z ((42))))))

; multiplication

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

; choice

(test-equal #t (term (≤ (choice ((1)) ((2))) ((1) (2)))))

; choiceEnv

(test-equal
 (term (choiceEnv
        ((*aVar ((5) (7))) (aVar ((7) (5))))
        ((*aVar ((7) (5))) (aVar ((2) (7) (5))))))
 (term ((*aVar ((7) (5))) (aVar ((2) (7) (5))))))

(test-equal
(term (choiceEnv ((CONDI ((6)))  (B ((3))) (A ((1)))) ((CONDI ((6))) (A ((1))) (B ((2))))) )
(term ((CONDI ((6))) (A ((1))) (B ((2))))))

(test-equal
(term (choiceEnv ((CONDI ((6))) (A ((1))) (B ((3)))) ((CONDI ((6))) (A ((1))) (B ((2))))) )
(term ((CONDI ((6))) (A ((1))) (B ((2) (3))))))

(test-equal (term (choiceEnv ((aVar ((5)))) ((aVar ((5)))))) (term ((aVar ((5))))))

; ⊑ check for COB types

(test-equal #t (term (⊑ (1 3 2 4) (4 3 5 2 6 1))))
(test-equal #f (term (⊑ (1 3 2 7) (4 3 5 2 6 1))))

; ≤ check for COB types

(test-equal #t (term (≤ () ((1)))))
(test-equal #t (term (≤ ((1 2) (3)) ((1 2 3)))))
(test-equal #t (term (≤ (choice ((1)) ((2))) ((1) (2)))))
(test-equal #t (term (≤ ((1) (2)) ((1 2)))))
(test-equal #t (and (term (≤ ((1) (2) (1 2)) ((1 2)))) (term (≤ ((1 2)) ((1) (2) (1 2)))))) ; ≤ not antisymetric
(test-equal #f (term (≤ ((3)) ((1)))))
(test-equal #f (term (≤ ((3)) ())))
(test-equal #f (term (≤ ((3 2)) ((3)))))

; choice

(test-equal
 (term (choice ((5 6)) ((1 2) (3 4))))
 (term ((3 4) (1 2) (5 6))))


; evalT

(test-equal ; simple num case
 (term (evalT
        ()
        (num 3)))
 (term ()))
(test-equal ; variable lookup
 (term (evalT
        ((y ((1 9))) (x ((2 4 5))))
        x))
 (term ((2 4 5))))
(test-equal ; addition
 (term (evalT
        ((y ((1 9))) (x ((2 4 8))))
        ((num 3) + x)))
 (term ((2 4 8))))



(println "All tests ran successfully.")