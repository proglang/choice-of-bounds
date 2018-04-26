#lang racket


(require redex  "Grammar.rkt" "Typecheck.rkt" rackunit)

(provide ⟿ >)

; The two judgment forms ⟿ and > translate a command into a command with explicit label handling.

; ⟿ does explicit lable handling for expressions.
(define-judgment-form VSIDO 
  #:mode     (⟿ I I I O O)
  #:contract (⟿ Γ E : T C)
  [
   --------------------------- X-INT
   (⟿ _ N_1 : () ((lab N_1) := ()))]

  [
   --------------------------- X-VAR
   (⟿ Γ V : (lookup Γ V) skip)]

  [ (⟿ Γ_1 E_1 : T_1 C_1)
    (⟿ Γ_1 E_2 : T_2 C_2)
   --------------------------- X-ARITH
   (⟿ Γ_1 (E_1 + E_2) : (multiplication T_1 T_2) ((C_1 then C_2) then ((lab (E_1 + E_2)) := ((lab E_1) ∪ (lab E_2)))))]

  [ (⟿ Γ_1 E_1 : Γ_1 C_1)
   --------------------------- X-DECLASS
   (⟿ Γ_1 (decl E_1 LAB_1 LAB_2 LAB_3) :
      (declassify Γ_1 LAB_1 LAB_2 LAB_3)
      ((C_1) then ((lab E_1) := (liftToLUB (declassify (lookup Γ_1 (lab(E_1))) LAB_1 LAB_2 LAB_3)))))])

; liftToLUB represents the meaning of a COB type: The least upper bound of each of its inner sets.
; Since the implementation assumes that the lattice is a powerset ordering, this liftToLUB simply
; returns the COB type. 
(define-metafunction VSIDO
  liftToLUB : T -> T
  [(liftToLUB T) T])

; Map an expression E to a metavariable lab(E).
; This implementation differs from the semantics in the paper in that
; 1) the is no explicit mapping from each expression and subexpression to a program point
; 2) the resulting var is not guaranteed to be unique.
(define-metafunction VSIDO
  lab : E -> V
  [(lab V)              ,(string->symbol (string-append "*"   (symbol->string (term V))))]
  [(lab (num number_1)) ,(string->symbol (string-append "**"  (number->string (term number_1))))]
  [(lab (E_1 + E_2))    ,(string->symbol (string-append "("   (symbol->string (term (lab E_1))) "+" (symbol->string(term (lab E_2))) ")"))]
  [(lab (dcl E _ _ _))  ,(string->symbol (string-append "***" (symbol->string (term (lab E)))))])

; Does explicit lable handling for commands. A derivation from the paper is that every assign is considered a dynamic assign.
; So this translation will always output a runtime check along with an assign which will get optimized away in cases where the rules in the paper
; could statically emit a assign statement in some cases. 
(define-judgment-form VSIDO 
  #:mode     (> I I I I I O O)
  #:contract (> Σ Γ T C : Γ C)

  [(⟿ Γ_1 E_1 : T_1 C_1)
   --------------------------- X-ASSIGN-DYN
   (> _ Γ_1 T_c (V_1 := E_1) :
      (ext Γ_1 V_1 (multiplication T_c T_1))
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
      Γ_2
      (C_2 then (let var X_1 := E_1 in (let var (lab X_1) := (lab E_1) in C_3))))]

  [(> Σ_1 Γ_1 T_c C_1 : Γ_2 C_3)
   (> Σ_1 Γ_2 T_c C_2 : Γ_3 C_4)
   --------------------------- X-SEQ
   (> Σ_1 Γ_1 T_c (C_1 then C_2) :
      Γ_3
      (C_3 then C_4))]

  [(⟿ Γ_1 E_1 : T_1 C_3)
   (> Σ_1 Γ_1 (multiplication T_c T_1) C_1 : Γ_2 C_4)
   (> Σ_1 Γ_1 (multiplication T_c T_1) C_2 : Γ_3 C_5)
   --------------------------- X-IF
   (> Σ_1 Γ_1 T_c (if (E_1) {C_1} else {C_2}) :
      (choiceEnv Γ_2 Γ_3)
      (C_3 then (let var pc := (pc ∪ (lab E_1)) in (if (E_1) {C_4} else {C_5}))))]

  [(where Γ_3 (findWhileEnv Σ_1 ((placeholderVarToMakeUniqueEnv98123797$&§?=§$=&1 ())) T_c (while (E_1) do {C_1}) Γ_1))
   (⟿ Γ_3 E_1 : T_1 C_expr)
   (> Σ_1 Γ_3 (multiplication T_c T_1) C_1 : Γ_2 C_body)
   --------------------------- X-WHILE
   (> Σ_1 Γ_1 T_c (while (E_1) do {C_1}) :
      Γ_2
      (while (E_1) do {(C_expr then (let var pc := (pc ∪ (lab E_1)) in C_body))}))   ]
  
  [
   --------------------------- X-SKIP
   (> _ Γ_1 _ skip : Γ_1 skip)]

  [
   --------------------------- X-HALT
   (> _ Γ_1 _ halt : Γ_1 halt)])