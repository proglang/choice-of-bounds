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
   (⟿ _ N_1 : (()) (***x := ()))] ; for now, assign to a variable that is never used. Ensure uniqueness later.

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
      ((C_1) then ((lab E_1) := (blub (declassify (expr-to-LABS Γ_1 E_1) LAB_1 LAB_2 LAB_3)))))])

; TODO stub, make right. Involves computing upper bound.
(define-metafunction VSIDO
  blub : T -> LABS
  [(blub T) (1 2 3)])

(define-metafunction VSIDO
 expr-to-LABS : μ E -> LABS
  [(expr-to-LABS _ N) ()]
;
  )


(define-metafunction VSIDO ; TODO ensure uniqueness for top-level exprs!
  lab : E -> V
  [(lab E) ,(string->symbol (term (labhelper E)))])
; need expr-to-LABS helper to lift into P(Labels)

(define-metafunction VSIDO ; TODO ensure uniqueness for top-level exprs!
  labhelper : E -> string
  [(labhelper V) ,(string-append "*" (symbol->string (term V)))]
  [(labhelper (num number_1)) ,(string-append "**" (number->string (term number_1)))]
  [(labhelper (E_1 + E_2)) ,(string-append "(" (term (labhelper E_1)) "+" (term (labhelper E_2)) ")")]
  [(labhelper (dcl E _ _ _)) ,(string-append "***" (term (labhelper E)))])



;(println "Metafunc Test: lab")
;(term (lab x))
;(term (lab (num 2)))
;(term (lab ((num 2) + (num 4))))
;
;(println "Test: Expressions Number Command")
;(judgment-holds
; (⟿ () (num 42) : T C)
; C)
;
;(println "Test: Expressions Var Command")
;(judgment-holds
; (⟿ ((x ((1 2) (3)))) x : T C)
; C)

(define-judgment-form VSIDO 
  #:mode     (> I I I I I O O)
  #:contract (> Σ Γ T C : Γ C)

  ; Ommit X-ASSIGN for now; assume every type is present at runtime
  [(⟿ Γ_1 E_1 : T_1 C_1)
   --------------------------- X-ASSIGN-DYN
   (> _ Γ_1 T_c (V_1 := E_1) :
      (ext Γ_1 V_1 (multiplication T_1 T_c))
      (if((pc ⊆ (lab V_1)))
         {(C_1 then (((lab V_1) := (pc ∪ (lab E_1))) then (V_1 := E_1)))}
         else
         {halt}))]
  
  [(⟿ Γ_1 E_1 : T_1 C_1)
   (side-condition (≤ (multiplication T_1 T_c) (lookup Σ_1 P_1)))
   --------------------------- X-OUT
   (> Σ_1 Γ_1 T_c (out(P_1 < E_1)) :
      Γ_1
      (out(P_1 < E_1)))]

  [(⟿ Γ_1 E_1 : T_1 C_1)
   (side-condition ,(not (term (≤ (multiplication T_1 T_c) (lookup Σ_1 P_1)))))
   (where LABS_1 (blub (lookup Σ_1 P_1)))
   --------------------------- X-OUT-DYN
   (> Σ_1 Γ_1 T_c (out(P_1 < E_1)) :
      Γ_1
      (C_1 then (if(((pc ∪ (lab E_1)) ⊆ LABS_1))
                     {(out(P_1 < E_1))} else
                     {halt})))]



)
