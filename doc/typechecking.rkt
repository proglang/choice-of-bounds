#lang racket

(require redex "../src/VSIDOT.rkt" rackunit)


(println "out - simple - success")
(judgment-holds
 (▷
  (((port 1) ((1 2 3 10))))
  ((aVar ((6))))
  ((3))
  (out((port 1) 🡐 ((num 3) :: ((1 2 3)))))
  : Γ)
 Γ)
(println "out - simple - failure. An empty environment means that there is no environment in which this judgement holds.")
(judgment-holds
 (▷
  (((port 1) ((1  3 10))))
  ((aVar ((6))))
  ((3))
  (out((port 1) 🡐 ((num 3) :: ((1 2 3)))))
  : Γ)
 Γ)
(println "out - indirect - success")
(judgment-holds
 (▷
  (((port 1) ((1 2 6))))
  ((CONDI ((6))) (A ((1))) (B ((2))))
  ()
  (if(CONDI)
     {(let var y := ((num 3) :: ((1 2 3))) in (out((port 1) 🡐 A)))} else
     {(out((port 1) 🡐 B))})
  : Γ)
 Γ)
; Todo: Should fail with an error instead
(println "out - indirect - failure. An empty environment means that there is no environment in which this judgement holds.")
(judgment-holds
 (▷
  (((port 1) ((1 2 6))))
  ((CONDI ((5))) (A ((1))) (B ((2))))
  ()
  (if(CONDI)
     {(() then (out((port 1) 🡐 A)))} else
     {(out((port 1) 🡐 B))})
  : Γ)
 Γ)



(term (choiceEnv ((CONDI ((6)))  (B ((3))) (A ((1)))) ((CONDI ((6))) (A ((1))) (B ((2))))) )

(term (choiceEnv ((CONDI ((6))) (A ((1))) (B ((3)))) ((CONDI ((6))) (A ((1))) (B ((2))))) )


(term (multiplication (()) ((5))))

(test-equal (term (choiceEnv ((aVar ((5)))) ((aVar ((5)))))) (term ((aVar ((5))))))



;
;(println "------------------ Test: Metafunctions")
;(println "Test: ⊑")
;(term (⊑ (1 3 2 4) (4 3 5 2 6 1)))
;(check-true (term (⊑ (1 3 2 4) (4 3 5 2 6 1))))
;(check-false (term (⊑ (1 3 2 7) (4 3 5 2 6 1))))
;
;(redex-match? VSIDOT LAB (term 3))
;
;
;
;(redex-match? VSIDOT Σ (term ()))
;(redex-match? VSIDOT Γ (term (((loc 1) ((4 5 7))))))
;(redex-match? VSIDOT T (term (())))
;(redex-match? VSIDOT M (term ((loc 3) := ((num 3) :: ((1 2 3))))))
;
;(println "------------------ Test: Type judgments")
;(println "assign - introduce new location")
;(judgment-holds
; (▷ () (((loc 1) ((4 5 7)))) (()) ((loc 3) := ((num 3) :: ((1 2 3)))) : Γ)
; Γ)
;(println "assign - update location")
;(judgment-holds
; (▷ () (((loc 3) ((4 5 7)))) (()) ((loc 3) := ((num 3) :: ((1 2 3)))) : Γ)
; Γ)
;(println "out - success")
;(judgment-holds
; (▷
;  (((port 1) ((10))))
;  ()
;  ((3))
;  (out((port 1) 🡐 ((num 3) :: ((1 2 3)))))
;  : Γ)
; Γ)
;(println "out - failure") ; TODO: should fail
;(judgment-holds
; (▷ (((port 1) (()))) () ((3)) (out((port 1) 🡐 ((num 3) :: ((1 2 3))))) : Γ)
; Γ)
;;(println "let")
;;(judgment-holds
;; (▷ () () () (let var "x" := ((num 3) :: ((1 2 3))) in ((loc 3) := "x")) : Γ)
;; Γ)
;(println "seq")
;(judgment-holds
; (▷ () (((loc 1) ((4)))) ((9)) (((loc 1) := ((num 3) :: ((3)))) then ((loc 1) := ((num 3) :: ((2))))) : Γ)
; Γ)
;(println "if")
;
;(term (choice ((9 5 2)) ((9 5 1))))
;
;(judgment-holds
; (▷ () (((loc 1) ((4)))) ((9)) (if (((num 3) :: ((5))))
;                                   {((loc 1) := ((num 42) :: ((2))))} else
;                                   {((loc 1) := ((num 42) :: ((1))))}) : Γ)
; Γ)
;(println "while")
;
;(println "------------------ Test: Metafunctions")
;(println "Test: ⊑")
;(check-true (term (⊑ (1 3 2 4) (4 3 5 2 6 1))))
;(check-false (term (⊑ (1 3 2 7) (4 3 5 2 6 1))))
;;(println "Test: ≤")
;;(check-true (term (≤ (()) ((1)))))
;;(check-true (term (≤ ((1 2) (3)) ((1 2 3)))))
;;(check-true (term (≤ (choice ((1)) ((2))) ((1) (2)))))
;;(check-true (term (≤ ((1) (2)) ((1 2)))))
;;(check-true (and (term (≤ ((1) (2) (1 2)) ((1 2)))) (term (≤ ((1 2)) ((1) (2) (1 2)))))) ; ≤ not antisymetric
;;(check-false (term (≤ ((3)) ((1)))))
;;(check-false (term (≤ ((3)) (()))))
;;(check-false (term (≤ ((3 2)) ((3)))))
;;
;;
;;
;;(term (choice ((1 2) (3 4)) ((5 6))))
;;(term (multiplication ((1 2) (3 4)) ((5 6))))
;;(term (multiplication ((1 2) (3 4)) ((5 6) (7 8) (9))))
;
;(test-equal
; (term (choice ((1 2) (3 4)) ((5 6))))
; (term ((1 2) (3 4) (5 6))))
;
;(redex-match? VSIDOT E (term (((num 1) :: ((1 2 3) (4 5))) + ((num 2) :: ((11 3) (8))))))
;(redex-match? VSIDOT LAB (term (12)))
;(redex-match? VSIDOT LAB (term 12 ))
;
;
;(term (declassify
;       (1 2 3)
;       (1 2 3)
;       (1 2 3)
;       (1 2 3)))
;;
;;;; evalT
;;(test-equal ; simple num case
;; (term (evalT
;;        ()
;;        (num 3)))
;; (term (())))
;;(test-equal ; variable lookup
;; (term (evalT
;;        (("y" ((1 9))) ("x" ((2 4 5))))
;;        "x"))
;; (term ((2 4 5))))
;;(test-equal ; addition
;; (term (evalT
;;        (("y" ((1 9))) ("x" ((2 4 8))))
;;        ((num 3) + "x")))
;; (term ((2 4 8))))
