#lang racket
(require redex "Grammar.rkt" rackunit)

(provide ▷ choice choiceEnv declassify ⊑ ≤ multiplication evalT findWhileEnv)


(define-judgment-form VSIDO ; type judgements for commands
  #:mode (▷ I I I I I O)
  #:contract (▷ Σ Γ T M : Γ)
  
  [
   --------------------------- ASSIGN
   (▷ Σ Γ_1 T_π (V_1 := E_1) : (ext Γ_1 V_1 (multiplication T_π (evalT Γ_1 E_1))))]

  [(side-condition ; If the side condition does not apply, an error should be thrown! use where/error from a newer redex version for this
     (≤
      (multiplication T_π (evalT Γ_1 E_1))
      (lookup Σ P_1)))
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
   (▷ Σ Γ_1 T_π (if (E_1) {C_1} else {C_2}) : (choiceEnv Γ_2 Γ_3))]

  [(▷ Σ Γ_1 T_π C_1 : Γ_2)
   --------------------------- WHILE
   (▷ Σ Γ_1 T_π (while (E_1) do {C_1}) : (findWhileEnv Σ Γ_1 T_π (while (E_1) do {C_1}) Γ_2))]

  [
   --------------------------- SKIP
   (▷ _ Γ_1 _ skip : Γ_1)]

  [
   --------------------------- HALT
   (▷ _ Γ_1 _ halt : Γ_1)])

; Overapproximate the type environment that would be produced by repeated execution of a while loop. 
(define-metafunction VSIDO
  findWhileEnv : Σ Γ T C Γ -> Γ
  [(findWhileEnv _ Γ_old _ _ Γ_new)
   Γ_new
   (side-condition (equal? (term Γ_old) (term Γ_new)))]
  [(findWhileEnv Σ_1 Γ_old T_1 C_1 Γ_new)
   (findWhileEnv Σ_1 Γ_new T_1 C_1 Γ_newer)
   (where Γ_newer ,(first (judgment-holds (▷ Σ_1 Γ_new T_1 C_1 : Γ) Γ)))])

; Decide whether one inner set is subset of the other inner set.
(define-metafunction VSIDO
  ⊑ : (LAB ...) (LAB ...) -> boolean
   [(⊑ (name lhs (LAB_0 ...)) (name rhs (LAB_1 ...)))
   ,(not
    (false?
     (for/and
         ([lhs-elem (term lhs)])
       (member lhs-elem (term rhs)))))])

; Decide whether a given type is contained in another type. 
(define-metafunction VSIDO
  ≤ : T T -> boolean
  [(≤ T_1 T_2)
   ,(for/and
        ([i (term T_1)])
        (for/or
            ([j (term T_2)])
            (term (⊑ ,i ,j))))])

; The sum of two types.
(define-metafunction VSIDO
  choice : T T -> T
  [(choice T_1 T_2) ,(set-union (term T_1) (term T_2))])

; The sum of two type environments. If a variable is bound in both environments, merge the two old bindings.
(define-metafunction VSIDO
  choiceEnv : Γ Γ -> Γ
  [(choiceEnv Γ_1 Γ_2)
   ,(foldl
    (lambda (lelem result)
      (term (ext Γ_2
           ,(first lelem)
           ,(set-union (secondTotal lelem) (secondTotal (assocTotal (first lelem) (term Γ_2)))))))
    '()
    (term Γ_1))])

; product operator: Given COB type A and B, A * B is the set of the pairwise sum of the inner sets of A and B.
; [ (1 2)     ] [ (4 5) (6 7) ] -> [ (1 2 4 5) (1 2 6 7)                 ]
; [ (1 2) (3) ] [ (4 5) (6 7) ] -> [ (1 2 4 5) (1 2 6 7) (3 4 5) (3 6 7) ]
(define-metafunction VSIDO
  multiplication : T T -> T
  [(multiplication T_1 ()) T_1]
  [(multiplication () T_2) T_2]
  [(multiplication T_1 T_2)
   ,(map flatten (cartesian-product (term T_1) (term T_2)))])

; Declassify a COB type
(define-metafunction VSIDO 
  declassify : T LAB LAB LAB -> T
  [(declassify T LAB_A LAB_B LAB_C)
   ,(map
    (lambda (i) (term (declassify-on-sets ,i LAB_A LAB_B LAB_C)))
    (term T))])

; Declassify a set of labels (equivalenty, a inner set of a COB type)
(define-metafunction VSIDO 
  declassify-on-sets : (LAB ...) LAB LAB LAB -> (LAB ...)
  [(declassify-on-sets (name inner-set (LAB ...)) LAB_A LAB_B LAB_C)
   ,(map
    (lambda (x)
     (cond
       ; Instead of looking at the ordering ⊑, simply use the ordering < of the natural numbers for now.
        [(and (< x (term LAB_A)) (< x (term LAB_B)) (not (< x (term LAB_C)))) (term LAB_C)]
        [else x]))
    (term inner-set))])

; Eval an expression to a type.
(define-metafunction VSIDO
  evalT : Γ E -> T
  [(evalT _ N)     ()]
  [(evalT _ T) T]
  [(evalT (_ ... (V T) _ ...) V) T]
  [(evalT Γ_1 (dcl E_1 LAB_A LAB_B LAB_C))
   (map
    (lambda (lables)
      (declassify lables LAB_A LAB_B LAB_C))
    (evalT Γ_1 E_1))]
  [(evalT Γ_1 (M_1 + M_2))
   (multiplication
    (evalT Γ_1 M_1)
    (evalT Γ_1 M_2))]
  [(evalT Γ_1 (E_1 ∪ E_2))
   ,(set-union
     (term (evalT Γ_1 E_1))
     (term (evalT Γ_1 E_2)))])