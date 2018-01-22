#lang racket

(require redex  "../src/Grammar.rkt" "../src/Optimize.rkt" rackunit)

; ORD-L
(test-equal
 (term (optimize-expression ((x ((1 5)))) (x ⊆ pc)))
 '(((1 5)) ⊆ pc))
; ORD-R
(test-equal
 (term (optimize-expression ((x ((1 5)))) (pc ⊆ x)))
 '(pc ⊆ ((1 5))))
; ORD-TRUE
(test-equal
 (term (optimize-expression ((x ((1) (2 3)))(y ((1 2 3) (1 4 3 2)))) (x ⊆ y)))
 '(num 1))
; ORD-FALSE
(test-equal
 (term (optimize-expression ((x ((1 4 3 2) (1 2 3)))(y ((1) (2 3)))) (x ⊆ y)))
 '(num 0))
; LUB-L
(test-equal
 (term (optimize-expression ((x ((1 4 3 2)))(y ((1) (2 3)))) (x ⊆ y)))
 (term (((1 4 3 2)) ⊆ y)))
; LUB-R
(test-equal
 (term (optimize-expression ((x ((1 4 3 2) (1 2 3)))(y ((2 3)))) (x ⊆ y)))
 (term (x ⊆ ((2 3)))))
; LUB-LSUBR
(test-equal
 (term (optimize-expression ((x ((1 3)))(y ((1 2 3) (1 3 5)))) (x ∪ y)))
 (term y))
; LUB-RSUBR
(test-equal
 (term (optimize-expression ((x ((1 4)))(y ((1 2 3) (1 3 5)))) (x ∪ y)))
 (term x))

;; METAFUNCTIONS

; comparison-always-true?
(check-true (term (comparison-always-true? () ((2 1)))))
(check-true (term (comparison-always-true? ((1) (2 3)) ((1 2 3)))))
(check-true (term (comparison-always-true? ((1) (2 3)) ((1 2 3) (1 4 3 2)))))
(check-false (term (comparison-always-true? ((2 1)) ())))
(check-false (term (comparison-always-true?  ((1 2 3)) ((1) (2 3)))))

; comparison-always-false?
(check-true (term (comparison-always-false? ((2 1)) ())))
(check-true (term (comparison-always-false?  ((1 2 3)) ((1) (2 3)))))
(check-false (term (comparison-always-false? () ((2 1)))))
(check-false (term (comparison-always-false? ((1) (2 3)) ((1 2 3)))))
(check-false (term (comparison-always-false? ((1) (2 3)) ((1 2 3) (1 4 3 2)))))

(println "All tests run successfully.")