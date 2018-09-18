#lang racket


(require redex  "Grammar.rkt" "Typecheck.rkt" rackunit)

(provide ⟿ > transformToExplicitLabels untagExpressionsOfProgram lab)

#|
Main method for translating a program into a program with explicit lable handling.
It adds unique tags for each expression and subexpression, adds the labels and remove the tags again.
|#

(define-metafunction VSIDO
  transformToExplicitLabels : Σ Γ T C -> C
  [(transformToExplicitLabels Σ_1 Γ_1 T_1 C_1) (untagExpressionsOfProgram ,(first (judgment-holds (> Σ_1 Γ_1 () (tagExpressionsOfProgram C_1) : _ C_2) C_2)))])

#|
Judgment forms for adding explicit label handling.
⟿ handles expressions, > commands.
|#

(define-judgment-form VSIDO 
  #:mode     (⟿ I I I O O)
  #:contract (⟿ Γ E : T C)
  [
   --------------------------- X-INT
   (⟿ _ (N_1 :: V_1) : () (V_1 := ()))]

  [
   --------------------------- X-VAR
   (⟿ Γ V_1 : (lookup Γ V_1) skip)]

  [ (⟿ Γ_1 (E_1 :: V_1) : T_1 C_1)
    (⟿ Γ_1 (E_2 :: V_2) : T_2 C_2)
   --------------------------- X-ARITH
   (⟿ Γ_1 (((E_1 :: V_1) + (E_2 :: V_2)) :: V_3) :
      (multiplication T_1 T_2)
      ((C_1 then C_2) then (V_3 := (V_1 ∪ V_2))))]

  [ (⟿ Γ_1 (E_1 :: V_1) : Γ_1 C_1)
   --------------------------- X-DECLASS
   (⟿ Γ_1 (decl (E_1 :: V_1) LAB_1 LAB_2 LAB_3) :
      (declassify Γ_1 LAB_1 LAB_2 LAB_3)
      ((C_1) then (V_1 := (liftToLUB (declassify (lookup Γ_1 V_1) LAB_1 LAB_2 LAB_3)))))])


; A derivation from the paper is that every assign is considered a dynamic assign.
; So this translation will always output a runtime check along with an assign which will get optimized away in cases
; where the rules in the paper could emit a non-dynmaic assign statement in some cases. 
(define-judgment-form VSIDO 
  #:mode     (> I I I I I O O)
  #:contract (> Σ Γ T C : Γ C)

  [(⟿ Γ_1 E_1 : T_1 C_1)
   (where T_2 (multiplication T_c T_1))
   --------------------------- X-ASSIGN-DYN
   (> _ Γ_1 T_c (V_1 := E_1) :
      (ext (ext Γ_1 V_1 T_2) (lab V_1) T_2) 
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
  [(where Γ_3 (findWhileEnv Σ_1 () T_c (while (E_1) do {C_1}) Γ_1))
   (⟿ Γ_3 E_1 : T_1 C_expr)
   (> Σ_1 Γ_3 (multiplication T_c T_1) C_1 : Γ_2 C_body)
   --------------------------- X-WHILE
   (> Σ_1 Γ_1 T_c (while (E_1) do {C_1}) :
      Γ_2
      (while (E_1) do {(C_expr then (let var pc := (pc ∪ (lab E_1)) in C_body))}))]
  
  [
   --------------------------- X-SKIP
   (> _ Γ_1 _ skip : Γ_1 skip)]

  [
   --------------------------- X-HALT
   (> _ Γ_1 _ halt : Γ_1 halt)])

; liftToLUB represents the meaning of a COB type: The least upper bound of each of its inner sets.
; Since the implementation assumes that the lattice is a powerset ordering, this liftToLUB simply
; returns the COB type. 
(define-metafunction VSIDO
  liftToLUB : T -> T
  [(liftToLUB T) T])

; Map an expression E to a metavariable lab(E).
(define-metafunction VSIDO
  lab : E -> V
  [(lab (_ :: V_1)) V_1]
  [(lab V_1) ,(string->symbol (string-append "labelvar-" (symbol->string (term V_1))))])

#|
Metafunctions to tag and untag expressions so that there is a one-to-one correspondance between tags and expressions.
|#

(define-metafunction VSIDO
  makeExprExplicit : any -> any
  [(makeExprExplicit E_1) (E_1 :: ,(generateNewLabel))])

(define-metafunction VSIDO
  tagExpressionsOfProgram : C -> C
  [(tagExpressionsOfProgram (V_1 := E_1)) (V_1 := (tagExpression E_1))]
  [(tagExpressionsOfProgram (out(P < E_1))) (out(P < (tagExpression E_1)))]
  [(tagExpressionsOfProgram (if (E_1) {C_1} else {C_2})) (if ((tagExpression E_1)) {(tagExpressionsOfProgram C_1)} else {(tagExpressionsOfProgram C_2)})]
  [(tagExpressionsOfProgram (while (E_1) do {C})) (while ((tagExpression E_1)) do {(tagExpressionsOfProgram C)})]
  [(tagExpressionsOfProgram (let var X := E_1 in C_1)) (let var X := (tagExpression E_1) in (tagExpressionsOfProgram C_1))]
  [(tagExpressionsOfProgram (C_1 then C_2)) ((tagExpressionsOfProgram C_1) then (tagExpressionsOfProgram C_2))]
  [(tagExpressionsOfProgram skip) skip]
  [(tagExpressionsOfProgram halt) halt])

(define-metafunction VSIDO
  tagExpression : E -> LabelledE
  [(tagExpression (E_1 + E_2)) (makeExprExplicit ((tagExpression E_1) + (tagExpression E_2)))]
  [(tagExpression (dcl E_1 LAB_1 LAB_2 LAB_3)) (makeExprExplicit (dcl (tagExpression E_1) LAB_1 LAB_2 LAB_3))]
  [(tagExpression (E_1 ∪ E_2)) (makeExprExplicit ((tagExpression E_1) ∪ (tagExpression E_2)))]
  [(tagExpression (E_1 ⊆ E_2)) (makeExprExplicit ((tagExpression E_1) ⊆ (tagExpression E_2)))]
  [(tagExpression N_1) (makeExprExplicit N_1)]
  [(tagExpression V_1) V_1]
  [(tagExpression T_1) (makeExprExplicit T_1)])

(define-metafunction VSIDO
  untagExpressionsOfProgram : C -> C
  [(untagExpressionsOfProgram (V_1 := E_1)) (V_1 := (untagExpression E_1))]
  [(untagExpressionsOfProgram (out(P < E_1))) (out(P < (untagExpression E_1)))]
  [(untagExpressionsOfProgram (if (E_1) {C_1} else {C_2})) (if ((untagExpression E_1)) {(untagExpressionsOfProgram C_1)} else {(untagExpressionsOfProgram C_2)})]
  [(untagExpressionsOfProgram (while (E_1) do {C})) (while ((untagExpression E_1)) do {(untagExpressionsOfProgram C)})]
  [(untagExpressionsOfProgram (let var X := E_1 in C_1)) (let var X := (untagExpression E_1) in (untagExpressionsOfProgram C_1))]
  [(untagExpressionsOfProgram (C_1 then C_2)) ((untagExpressionsOfProgram C_1) then (untagExpressionsOfProgram C_2))]
  [(untagExpressionsOfProgram skip) skip]
  [(untagExpressionsOfProgram halt) halt])

(define-metafunction VSIDO
  untagExpression : E -> E
  [(untagExpression ((E_1 + E_2) :: _)) ((untagExpression E_1) + (untagExpression E_2)) ]
  [(untagExpression ((dcl E_1 LAB_1 LAB_2 LAB_3) :: _)) (dcl (untagExpression E_1) LAB_1 LAB_2 LAB_3)]
  [(untagExpression ((E_1 ∪ E_2) :: _)) ((untagExpression E_1) ∪ (untagExpression E_2))]
  [(untagExpression ((E_1 ⊆ E_2) :: _)) ((untagExpression E_1) ⊆ (untagExpression E_2))]
  [(untagExpression (N_1 :: _)) N_1]
  [(untagExpression (V_1 :: _)) V_1]
  [(untagExpression (T_1 :: _)) T_1]
  [(untagExpression any) any])

(define counter 0)

(define (generateNewLabel)
  (set! counter (+ counter 1))
  (string->symbol (string-append "labelvar-" (number->string counter))))






