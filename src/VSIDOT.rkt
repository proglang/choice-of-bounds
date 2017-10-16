#lang racket
(require redex "VSIDO.rkt" rackunit)

(define-extended-language VSIDOT VSIDO
  (T ::= (LAB ...))
  (LAB ::= (number ...))
  (E ::= ....
     (E :: T))
  (Γ ::= ((V T) ...)) ; variable type environment
  (Σ ::= ((P T) ...)))



(define-judgment-form VSIDOT ; type judgements for commands
  #:mode (▷ I I I I I O)
  #:contract (▷ Σ Γ T M : Γ)
  [
   --------------------------- ASSIGN
   (▷ Σ Γ_1 T_π (L_1 := E_1) : (extL Γ_1 L_1 (multiplication T_π (evalT Γ_1 E_1))))]
  [(side-condition
     (≤
      (multiplication T_π (evalT Γ_1 E_1))
      (lookupPortT Σ P_1)))
   --------------------------- OUT
   (▷ Σ Γ_1 T_π (out(P_1 🡐 E_1)) : Γ_1)]
  [
   --------------------------- LET
   (▷ Σ Γ_1 T_π (let var X_1 := E_1 in C_1) : (subst Γ_1 X_1 (evalT Γ_1 E_1)))]
  [(▷ Σ Γ_1 T_π C_1 : Γ_2)
   (▷ Σ Γ_2 T_π C_2 : Γ_3)
   --------------------------- SEQ
   (▷ Σ Γ_1 T_π (C_1 then C_2) : Γ_3)]
  [(where T_newContext (multiplication T_π (evalT Γ_1 E_1)))
   (▷ Σ Γ_1 T_newContext C_1 : Γ_2) (▷ Σ Γ_1 T_newContext C_2 : Γ_3)
   --------------------------- IF
   (▷ Σ Γ_1 T_π (if (E_1) {C_1} else {C_2}) : (choice Γ_2 Γ_3))]
  [
   --------------------------- WHILE
   (▷ Σ Γ_1 T_π (while (E_1) do {C_1}) : (sumWhileTypes Σ Γ_1 T_π E_1 C_1))])

(define-metafunction VSIDOT
  lookupPortT : Σ P -> T
  [(lookupPortT (any ... (P T) any ...) P) T])

(define-metafunction VSIDOT
  sumWhileTypes : Σ Γ T E C -> Γ)

(define-metafunction VSIDOT
  substT : Γ X T -> Γ)

(define-metafunction VSIDOT
  ⊑ : LAB LAB -> boolean
  [(⊑ LAB_1 LAB_2)
   ,(not
    (false?
     (for/and
         ([lhs-elem (term LAB_1)])
       (member lhs-elem (term LAB_2)))))])
(define-metafunction VSIDOT
  ≤ : T T -> boolean
  [(≤ T_1 T_2)
   ,(for/and
        ([i (term T_1)])
        (for/or
            ([j (term T_2)])
            (term (⊑ ,i ,j))))])

(define-metafunction VSIDOT
  choice : T T -> T
  [(choice T_1 T_2) ,(append (term T_1) (term T_2))])

(define-metafunction VSIDOT ; TODO: in progress
  choiceEnv : Γ Γ -> Γ
  [(choiceEnv T_1 T_2) ,(append (term T_1) (term T_2))])

(define-metafunction VSIDOT
  multiplication : T T -> T
  [(multiplication T_1 T_2)
   ,(let ([accumulator '()])
      (for-each (lambda (lhs-elem)
         (for-each (lambda (rhs-elem)
            (set!
             accumulator
             (append
              accumulator
              (cons (append lhs-elem rhs-elem) null))))
          (term T_2)))
       (term T_1))
      accumulator)])

(define-metafunction VSIDOT
  evalT : Γ E -> T ; TODO: ADD DECLASS
  [(evalT _ N)     (())]
  [(evalT _ (_ :: T)) T]
  [(evalT Γ_1 X_1) ,(second (assoc (term X_1) (term Γ_1)))]
  [(evalT Γ_1 (M_1 + M_2))
   (multiplication
    (evalT Γ_1 M_1)
    (evalT Γ_1 M_2))])


(println "------------------ Test: Type judgments")
(println "assign - introduce new location")
(judgment-holds
 (▷ () (((loc 1) ((4 5 7)))) (()) ((loc 3) := ((num 3) :: ((1 2 3)))) : Γ)
 Γ)
(println "assign - update location")
(judgment-holds
 (▷ () (((loc 3) ((4 5 7)))) (()) ((loc 3) := ((num 3) :: ((1 2 3)))) : Γ)
 Γ)
(println "out - success")
(judgment-holds
 (▷ (((port 1) ((10)))) () ((3)) (out((port 1) 🡐 ((num 3) :: ((1 2 3))))) : Γ)
 Γ)
(println "out - failure") ; TODO: should fail
(judgment-holds
 (▷ (((port 1) (()))) () ((3)) (out((port 1) 🡐 ((num 3) :: ((1 2 3))))) : Γ)
 Γ)
;(println "let")
;(judgment-holds
; (▷ () () () (let var "x" := ((num 3) :: ((1 2 3))) in ((loc 3) := "x")) : Γ)
; Γ)
(println "seq")
(judgment-holds
 (▷ () (((loc 1) ((4)))) ((9)) (((loc 1) := ((num 3) :: ((3)))) then ((loc 1) := ((num 3) :: ((2))))) : Γ)
 Γ)
(println "if")
(judgment-holds
 (▷ () (((loc 1) ((4)))) ((9)) (if (((num 3) :: ((5))))
                                   {((loc 1) := ((num 42) :: ((2))))} else
                                   {((loc 1) := ((num 42) :: ((1))))}) : Γ)
 Γ)
(println "while")

(println "------------------ Test: Metafunctions")
(println "Test: ⊑")
(check-true (term (⊑ (1 3 2 4) (4 3 5 2 6 1))))
(check-false (term (⊑ (1 3 2 7) (4 3 5 2 6 1))))
(println "Test: ≤")
(check-true (term (≤ (()) ((1)))))
(check-true (term (≤ ((1 2) (3)) ((1 2 3)))))
(check-true (term (≤ (choice ((1)) ((2))) ((1) (2)))))
(check-true (term (≤ ((1) (2)) ((1 2)))))
(check-true (and (term (≤ ((1) (2) (1 2)) ((1 2)))) (term (≤ ((1 2)) ((1) (2) (1 2)))))) ; ≤ not antisymetric
(check-false (term (≤ ((3)) ((1)))))
(check-false (term (≤ ((3)) (()))))
(check-false (term (≤ ((3 2)) ((3)))))



(term (choice ((1 2) (3 4)) ((5 6))))
(term (multiplication ((1 2) (3 4)) ((5 6))))
(term (multiplication ((1 2) (3 4)) ((5 6) (7 8) (9))))

(test-equal
 (term (choice ((1 2) (3 4)) ((5 6))))
 (term ((1 2) (3 4) (5 6))))

;; evalT
(test-equal ; simple num case
 (term (evalT
        ()
        (num 3)))
 (term (())))
(test-equal ; variable lookup
 (term (evalT
        (("y" ((1 9))) ("x" ((2 4 5))))
        "x"))
 (term ((2 4 5))))
(test-equal ; addition
 (term (evalT
        (("y" ((1 9))) ("x" ((2 4 8))))
        ((num 3) + "x")))
 (term ((2 4 8))))

