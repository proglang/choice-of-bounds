#lang racket

(require redex  "../src/Grammar.rkt" "../src/Optimize.rkt" rackunit)

; ORD-L
(test-equal
 (term (opt-expr ((x ((1 5))) (pc ((1 5) (7)))) (x ⊆ pc)))
 '(((1 5)) ⊆ pc))
; ORD-R
(test-equal
 (term (opt-expr ((x ((1 5))) (pc ((1 5) (7)))) (pc ⊆ x)))
 '(pc ⊆ ((1 5))))
; ORD-TRUE
(test-equal
 (term (opt-expr ((x ((1) (2 3)))(y ((1 2 3) (1 4 3 2)))) (x ⊆ y)))
 '(num 1))
; ORD-FALSE
(test-equal
 (term (opt-expr ((x ((1 4 3 2) (1 2 3)))(y ((1) (2 3)))) (x ⊆ y)))
 '(num 0))
; LUB-L
(test-equal
 (term (opt-expr ((x ((1 4 3 2)))(y ((1) (1 2 3 4)))) (x ⊆ y)))
 (term (((1 4 3 2)) ⊆ y)))
; LUB-R
(test-equal
 (term (opt-expr ((x ((1 4 3 2) (1 2 3))) (y ((1 2 3)))) (x ⊆ y)))
 (term (x ⊆ ((1 2 3)))))
; LUB-LSUBR
(test-equal
 (term (opt-expr ((x ((1 3)))(y ((1 2 3) (1 3 5)))) (x ∪ y)))
 (term y))
; LUB-RSUBR
(test-equal
 (term (opt-expr ((x ((1 2 3 4) (1 2 3 4 5)))(y ((1 2) (3 4)))) (x ∪ y)))
 (term x))


;; METAFUNCTIONS

; Test union
(test-equal
 (term (decideSums () (((8 10)) ∪ ((11 13)))))
 (term ((13 11 8 10))))

; Dynamic check uneccessary because always succeeds
(test-equal
 (term (opt-expr
       ((pc ((1 3))) (*x ((1 5))))
       ((pc ∪ *x) ⊆ ((1 3 5)))))
 (term (num 1)))


; Dynamic check uneccessary because always fails
(test-equal
 (term (opt-expr
       ((pc ((1 3))) (*x ((1 5))))
       (((1 3 5)) ⊆ *x)))
 (term (num 0)))

(test-equal
 (term (opt-expr
       ((pc ()) (*aVar ((5) (7))))
       ((pc ∪ *aVar) ⊆ ((5)))))
 (term (*aVar ⊆ ((5)))))


; Tests variable substitution
(test-equal
 (term (opt-expr ((x ((1 3)))) x))
 '((1 3)))


; eliminateDeadCode

(test-equal
 (term (eliminateDeadCode halt halt))
 (term halt))

(test-equal
 (term (eliminateDeadCode (skip then halt) halt))
 (term halt))

; comparison-always-true?

(test-equal #t (term (comparison-always-true? () ((2 1)))))
(test-equal #t (term (comparison-always-true? ((1) (2 3)) ((1 2 3)))))
(test-equal #t (term (comparison-always-true? ((1) (2 3)) ((1 2 3) (1 4 3 2)))))
(test-equal #f (term (comparison-always-true? ((2 1)) ())))
(test-equal #f (term (comparison-always-true?  ((1 2 3)) ((1) (2 3)))))

; comparison-always-false?

(test-equal #t (term (comparison-always-false? ((2 1)) ())))
(test-equal #t (term (comparison-always-false?  ((1 2 3)) ((1) (2 3)))))
;(check-false (term (comparison-always-false? () ((2 1)))))
(test-equal #f (term (comparison-always-false? ((1) (2 3)) ((1 2 3)))))
(test-equal #f (term (comparison-always-false? ((1) (2 3)) ((1 2 3) (1 4 3 2)))))

(println "All tests ran successfully.")