#lang racket


(require redex  "Grammar.rkt" "Typecheck.rkt" rackunit)

(provide ⟿ >)

; VSIDO+Labels extends VISDOT by adding explicit label handling to the language.
; Note that the (sub)-expressions have not yet been mapped at this point. Before evaluating a program
; in this language, this mapping has to done first in order to execute the lab-statements.

(define-judgment-form VSIDO 
  #:mode     (⟿ I I I O O)
  #:contract (⟿ Γ E : T C)
  [
   --------------------------- X-INT
   (⟿ _ N_1 : () skip)] ; TODO for now, skip.

  [
   --------------------------- X-VAR
   (⟿ Γ V : (lookup Γ V) skip)]

  [ (⟿ Γ_1 E_1 : Γ_2 C_1)
    (⟿ Γ_1 E_2 : Γ_3 C_2)
   --------------------------- X-ARITH
   (⟿ Γ_1 (E_1 + E_2) : (mulitiply Γ_2 Γ_3) ((C_1 then C_2) then ((lab (E_1 + E_2)) := ((lab E_1) ∪ (lab E_2)))))]

  [ (⟿ Γ_1 E_1 : Γ_1 C_1)
   --------------------------- X-DECLASS
   (⟿ Γ_1 (decl E_1 LAB_1 LAB_2 LAB_3) :
      (declassify Γ_1 LAB_1 LAB_2 LAB_3)
      ((C_1) then ((lab E_1) := (liftToLUB (declassify (expr-to-LABS Γ_1 E_1) LAB_1 LAB_2 LAB_3)))))])

; liftToLUB represents the meaning of a COB type: The least upper bound of each of its inner sets.
; Since the this implementation assumes that the lattice is a powerset ordering, this liftToLUB simply
; returns the type. 
(define-metafunction VSIDO
  liftToLUB : T -> T
  [(liftToLUB T) T])

; TODO complete
(define-metafunction VSIDO
 expr-to-LABS : μ E -> LABS
  [(expr-to-LABS _ N) ()])


(define-metafunction VSIDO ; TODO ensure uniqueness for top-level exprs!
  lab : E -> V
  [(lab E) ,(string->symbol (term (labhelper E)))])

(define-metafunction VSIDO ; TODO ensure uniqueness for top-level exprs!
  labhelper : E -> string
  [(labhelper V) ,(string-append "*" (symbol->string (term V)))]
  [(labhelper (num number_1)) ,(string-append "**" (number->string (term number_1)))]
  [(labhelper (E_1 + E_2)) ,(string-append "(" (term (labhelper E_1)) "+" (term (labhelper E_2)) ")")]
  [(labhelper (dcl E _ _ _)) ,(string-append "***" (term (labhelper E)))])

(define-judgment-form VSIDO 
  #:mode     (> I I I I I O O)
  #:contract (> Σ Γ T C : Γ C)

  ; Ommit X-ASSIGN for now; assume every type is present at runtime

  [(⟿ Γ_1 E_1 : T_1 C_1)
   --------------------------- X-ASSIGN-DYN
   (> _ Γ_1 T_c (V_1 := E_1) :
      (ext
       (ext
        (ext Γ_1 V_1 (multiplication T_1 T_c)) (lab V_1) (multiplication T_1 T_c)) (lab E_1) T_1)
      (if((pc ⊆ (lab V_1)))
         {(C_1 then (((lab V_1) := (pc ∪ (lab E_1))) then (V_1 := E_1)))}
         else
         {halt}))]
  
  [(⟿ Γ_1 E_1 : T_1 C_1)
   (side-condition (≤ (multiplication T_1 T_c) (lookup Σ_1 P_1)))
   --------------------------- X-OUT
   (> Σ_1 Γ_1 T_c (out(P_1 < E_1)) :
      (ext Γ_1 (lab E_1) T_1) 
      (out(P_1 < E_1)))]

  [(⟿ Γ_1 E_1 : T_1 C_1)
   (side-condition ,(not (term (≤ (multiplication T_1 T_c) (lookup Σ_1 P_1)))))
   (where T_2 (liftToLUB (lookup Σ_1 P_1)))
   --------------------------- X-OUT-DYN
   (> Σ_1 Γ_1 T_c (out(P_1 < E_1)) :
      (ext Γ_1 (lab E_1) T_1) 
      (C_1 then (if(((pc ∪ (lab E_1)) ⊆ T_2))
                     {(out(P_1 < E_1))} else
                     {halt})))]

  [(⟿ Γ_1 E_1 : T_1 C_2)
   (> Σ_1 (ext Γ_1 X_1 T_1) T_c C_1 : Γ_2 C_3)
   --------------------------- X-LET
   (> Σ_1 Γ_1 T_c (let var X_1 := E_1 in C_1) :
      (ext (ext Γ_2 (lab X_1) T_1) (lab E_1) T_1)
      (C_2 then (let var X_1 := E_1 in (let var (lab X_1) := (lab E_1) in C_3))))]

  [(> Σ_1 Γ_1 T_c C_1 : Γ_2 C_3)
   (> Σ_1 Γ_2 T_c C_2 : Γ_3 C_4)
   --------------------------- X-SEQ
   (> Σ_1 Γ_1 T_c (C_1 then C_2) :
      (choiceEnv (choiceEnv Γ_3 Γ_2) Γ_1)
      (C_3 then C_4))]

  [(⟿ Γ_1 E_1 : T_1 C_3)
   (> Σ_1 Γ_1 T_c C_1 : Γ_2 C_4)
   (> Σ_1 Γ_1 T_c C_2 : Γ_3 C_5)
   --------------------------- X-IF
   (> Σ_1 Γ_1 T_c (if (E_1) {C_1} else {C_2}) :
      (ext (choiceEnv Γ_2 Γ_3) (lab E_1) (multiplication T_c T_1))
      (C_3 then (let var pc := (pc ∪ (lab E_1)) in (if (E_1) {C_4} else {C_5}))))]

  [
   --------------------------- X-SKIP
   (> Σ_1 Γ_1 T_c skip : Γ_1 skip)]

  [
   --------------------------- X-HALT
   (> Σ_1 Γ_1 T_c halt : Γ_1 halt)])
