#lang racket

(require redex  "Grammar.rkt" "Typecheck.rkt" "Interprete.rkt" "../src/TransformToXLabels.rkt" "../src/Interprete.rkt" rackunit)

(provide ⟹ comparison-always-true?  eliminateDeadCode optimize eliminateDead comparison-always-false? opt-expr decideSums substVarsWithInnersets optimizeSkipAndHalts)



; Answer true iff for two given types T_1 and T_2: σ ∈ T_1 and τ ∈ T_2 . σ ⊆ τ
(define-metafunction VSIDO
  comparison-always-true? : T T -> boolean
  [(comparison-always-true? () _) #t]
  [(comparison-always-true? _ ()) #f]
  [(comparison-always-true? T_1 T_2)
   ,(andmap
     (lambda (subset_left)
       (andmap
        (lambda (subset_right)
          (subset? subset_left subset_right))
        (term T_2)))
     (term T_1))])

; Answer true iff for two given types T_1 and T_2: σ ∈ T_1 and τ ∈ T_2 . σ ⊈ τ
(define-metafunction VSIDO
  comparison-always-false? : T T -> boolean
  [(comparison-always-false? _ ()) #t]
  [(comparison-always-false? T_1 T_2)
   ,(andmap
     (lambda (subset_left)
       (andmap
        (lambda (subset_right)
          (not (subset? subset_left subset_right)))
        (term T_2)))
     (term T_1))])


; If a comparison will always be true or false at runtime, replace it with it respective numerical value. Example:
; decideComparison ((x) ((1 2)   (3 4)))   (x ⊆ ((1 2 3 4))) -> (num 1)
; decideComparison ((x) ((1 2 9) (3 4 9))) (x ⊆ ((1 2 3 4))) -> (num 0)
; decideComparison ((x) ((1 2 9) (3 4)))   (x ⊆ ((1 2 3 4))) -> (x ⊆ ((1 2 3 4)))
(define-metafunction VSIDO
  decideComparison : Γ E -> E
  [(decideComparison Γ_1 (() ⊆ VOrT_2)) (num 1)]
  [(decideComparison Γ_1 (VOrT_1 ⊆ ())) (num 0)]
  [(decideComparison Γ_1 (VOrT_1 ⊆ VOrT_2)) (num 1)
   (side-condition (term (comparison-always-true?
                          (lookupOrDefault Γ_1 VOrT_1)
                          (lookupOrDefault Γ_1 VOrT_2))))]
  [(decideComparison Γ_1 (VOrT_1 ⊆ VOrT_2)) (num 0)
   (side-condition (term (comparison-always-false?
                          (lookupOrDefault Γ_1 VOrT_1)
                          (lookupOrDefault Γ_1 VOrT_2))))]
  [(decideComparison _ E_1) E_1])


; Decide whether an union of runtime labels can be replaced with either of its operand labels.
; Corresponds to rules LUB-RSUBL and LUB-LSUBR
; Note: There are no nested unions of set lables - Unions exist only in one of two forms:
; 1. As the right-hand operator of an expression: V := (pc ∪ V)
; 2. As the left-hand operator of an comparison: ((pc ∪ V) ⊆ T)
(define-metafunction VSIDO
  decideSums : Γ E -> E
  [(decideSums Γ_1 (VOrT_1 ∪ VOrT_2)) VOrT_2
   (side-condition (term (comparison-always-true?
                          (lookupOrDefault Γ_1 VOrT_1)
                          (lookupOrDefault Γ_1 VOrT_2))))]
  [(decideSums Γ_1 (VOrT_1 ∪ VOrT_2)) VOrT_1
   (side-condition (term (comparison-always-true?
                          (lookupOrDefault Γ_1 VOrT_2)
                          (lookupOrDefault Γ_1 VOrT_1))))]
  [(decideSums _ ((LABS_1) ∪ (LABS_2))) (,(set-union (term LABS_1) (term LABS_2)))]
  [(decideSums Γ_1 (E_1 ⊆ E_2)) ((decideSums Γ_1 E_1) ⊆ E_2)]
  [(decideSums Γ_1 E_1) E_1])

; If a variable only represents a COB with a single choice, replace it with its inner set.
(define-metafunction VSIDO
  substVarsWithInnersets : Γ E -> E
  ; Base case: Replace Vars that only map to one inner set
  [(substVarsWithInnersets (_ ... (V_1 TR_1) _ ...) V_1) TR_1]
  ; DFS
  [(substVarsWithInnersets Γ_1 (E_1 ∪ E_2))
   ((substVarsWithInnersets Γ_1 E_1) ∪ (substVarsWithInnersets Γ_1 E_2))]
  [(substVarsWithInnersets Γ_1 (E_1 ⊆ E_2))
   ((substVarsWithInnersets Γ_1 E_1) ⊆ (substVarsWithInnersets Γ_1 E_2))]
  ; No optimization to left to do; return the same expression
  [(substVarsWithInnersets _ E_1) E_1])

; Helper metafuntion for optimizing expressions.
(define-metafunction VSIDO
  opt-expr : Γ E -> E
  [(opt-expr Γ_1 E_1)
   (decideComparison Γ_1
                     (decideSums Γ_1
                                 (substVarsWithInnersets Γ_1 E_1)))])

; Implement all optimization rules apart from ASSIGN-DEAD and LET-DEAD.
(define-judgment-form VSIDO 
  #:mode     (⟹ I I I I O O)
  #:contract (⟹ Γ T C : Γ C)
  [
   --------------------------- R-ASSIGN
   (⟹ Γ_1 T_c (V_1 := E_1) :
      (ext Γ_1 V_1 (multiplication T_c (evalT Γ_1 E_1)))
      (V_1 := (opt-expr Γ_1 E_1)))]

  [(where (num 1) (opt-expr Γ_1 E_1))
   (⟹ Γ_1 T_c C_1 : Γ_2 C_2)
   --------------------------- IF-TRUE
   (⟹ Γ_1 T_c (if (E_1) {C_1} else {C_5}) :
      Γ_2
      C_2)]

  [(where (num 0) (opt-expr Γ_1 E_1))
   (⟹ Γ_1 T_c C_1 : Γ_2 C_2)
   --------------------------- IF-FALSE
   (⟹ Γ_1 T_c (if (E_1) {C_5} else {C_1}) :
      Γ_2
      C_2)]

  [(where NonNumericalExpression (opt-expr Γ_1 E_1))
   (⟹ Γ_1 T_c C_1 : Γ_3 C_3)
   (⟹ Γ_1 T_c C_2 : Γ_4 C_4)
   --------------------------- IF-UNOPTIMIZED
   (⟹ Γ_1 T_c (if (E_1) {C_1} else {C_2}) :
      (choiceEnv Γ_3 Γ_4)
      (if (E_1) {C_3} else {C_4}))]

  
  [(⟹ Γ_1 T_c C_1 : Γ_2 C_3)
   (⟹ Γ_2 T_c C_2 : Γ_3 C_4)
   --------------------------- R-SEQ
   (⟹ Γ_1 T_c (C_1 then C_2) :
      Γ_3
      (C_3 then C_4))]

    [(⟹ (ext Γ_1 V_1 (evalT Γ_1 E_1)) T_c C_1 : Γ_2 C_2)
   --------------------------- R-LET
   (⟹ Γ_1 T_c (let var V_1 := E_1 in C_1) :
      Γ_2
      (let var V_1 := E_1 in C_2))]

  [(⟹ Γ_1 T_c C_1 : Γ_2 C_2)
   --------------------------- R-WHILE
   (⟹ Γ_1 T_c (while (E_1) do {C_1}) :
      Γ_2
      (while (E_1) do {C_2}))]

  [
   --------------------------- R-OUT
   (⟹ Γ_1 T_c (out(P < E)) : Γ_1 (out(P < E)))]

  [ (⟹ Γ_1 T_c skip : Γ_1 skip) ]
  [ (⟹ Γ_1 T_c halt : Γ_1 halt) ])

; Convenience function that makes the resulting program look nicer.
(define-metafunction VSIDO
  optimizeSkipAndHalts : C -> C
  [(optimizeSkipAndHalts (skip then C_1))
   (optimizeSkipAndHalts C_1)]
  [(optimizeSkipAndHalts (C_1 then skip))
   (optimizeSkipAndHalts C_1)]
  [(optimizeSkipAndHalts (halt then _))
   halt]
  [(optimizeSkipAndHalts (C_1 then C_2))
   ((optimizeSkipAndHalts C_1) then (optimizeSkipAndHalts C_2))]
  [(optimizeSkipAndHalts (if (E_1) {halt} else {halt}))
   halt]
  [(optimizeSkipAndHalts (if (E_1) {C_1} else {C_2}))
   (if (E_1) {(optimizeSkipAndHalts C_1)} else {(optimizeSkipAndHalts C_2)})]
  [(optimizeSkipAndHalts (let var X_1 := E_1 in C_1))
   (let var X_1 := E_1 in (optimizeSkipAndHalts C_1))]
  [(optimizeSkipAndHalts C_1) C_1])

; Implements the rules ASSIGN-DEAD and LETVAR-DEAD.
(define-metafunction VSIDO
  eliminateDead :
  C ; Command which is to be optimizied.
  (X ...) ; Variables that will be used after the input .
  -> C
  ; LETVAR-DEAD
  [(eliminateDead (let var X_1 := E_1 in C_1) any_ls)
   C_elim
   (where C_elim (eliminateDead C_1 any_ls))
   (side-condition (not (member (term X_1) (term (free C_elim)))))]
  ; ASSIGN-DEAD TODO: While-sicher!
  [(eliminateDead (X_1 := E_1) any_ls)
   skip
   (side-condition (not (member (term X_1) (term any_ls))))]
  [(eliminateDead (C_1 then C_2) any_contextVars)
   ((eliminateDead C_1 ,(set-union (term (free C_elim)) (term any_contextVars))) then
    C_elim)
   (where C_elim (eliminateDead C_2 any_contextVars))]
  [(eliminateDead (if (E_1) {C_1} else {C_2}) any_contextVars)
   (if (E_1) {(eliminateDead C_1 any_contextVars)} else {(eliminateDead C_2 any_contextVars)})]
  [(eliminateDead C_1 _)
   C_1])


; Helper function to iteratively eliminate dead code. The two subfunctions create new opportunities for each other:
; optimizeSkipAndHalts can end the life of an variable, for example: (halt then x := y) when this is the only use of y.
; eliminateDeadCode can create new skips and halts, for example: (x := y) => skip, when x is dead.
(define-metafunction VSIDO
  eliminateDeadCode : C C -> C
  [(eliminateDeadCode C_old C_new)
   C_old
   (side-condition (equal? (term C_old) (term C_new)))]
  [(eliminateDeadCode C_old C_new)
   (eliminateDeadCode C_new (optimizeSkipAndHalts (eliminateDead C_new ())))])

; First do all one-pass optimizations by executing the judgment ⟹, then do the optimizations that require repeated execution.
(define-metafunction VSIDO
  optimize : C Γ -> C
  [(optimize C_1 Γ_1)
   (eliminateDeadCode
        halt ; start value
        ,(first (judgment-holds (⟹ Γ_1 () C_1 : Γ C ) C)))])