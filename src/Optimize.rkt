#lang racket

(require redex  "Grammar.rkt" "Typecheck.rkt" "Interprete.rkt" "../src/TransformToXLabels.rkt" "../src/Interprete.rkt")

(provide ⟹ optimize-expression comparison-always-true? comparison-always-false?)




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

(define-metafunction VSIDO
  comparison-always-false? : T T -> boolean
  [(comparison-always-false? () _) #f]
  [(comparison-always-false? _ ()) #t]
  [(comparison-always-false? T_1 T_2)
   ,(andmap
     (lambda (subset_left)
       (andmap
        (lambda (subset_right)
          (not (subset? subset_left subset_right)))
        (term T_2)))
     (term T_1))])

(define-metafunction VSIDO
  optimize-expression : Γ E -> E
  [(optimize-expression Γ_1 E_1)
   E_3
   (where E_2 (optimize-expression-inner Γ_1 E_1))
   (where E_3 (optimize-expression-inner Γ_1 E_2))
   (side-condition (eq? (term E_2) (term E_3)))]
  [(optimize-expression Γ_1 E_1)
   (optimize-expression Γ_1 (optimize-expression-inner Γ_1 E_1))])

(define-metafunction VSIDO
  optimize-expression-inner : Γ E -> E

  ; LUB-LSUBR
  [(optimize-expression-inner Γ_1 (V_1 ∪ V_2))
   V_2
   (side-condition (term (comparison-always-true? (lookup Γ_1 V_1) (lookup Γ_1 V_2))))]
  ; LUB-RSUBR
  [(optimize-expression-inner Γ_1 (V_1 ∪ V_2))
   V_1
   (side-condition (term (comparison-always-false? (lookup Γ_1 V_1) (lookup Γ_1 V_2))))]
  ; ORD-L
  [(optimize-expression-inner (_ ... (V_1 (LABS_1)) _ ...) (V_1 ⊆ E_1))
   ((LABS_1) ⊆ E_1)]
  ; ORD-R
  [(optimize-expression-inner (_ ... (V_1 (LABS_1)) _ ...) (E_1 ⊆ V_1))
   (E_1 ⊆ (LABS_1))]
  
   ; LUB-L
  [(optimize-expression-inner (_ ... (V_1 (LABS_1)) _ ...) (V_1 ∪ E_1))
   ((LABS_1) ∪ E_1)]
  ; LUB-R
  [(optimize-expression-inner (_ ... (V_1 (LABS_1)) _ ...) (E_1 ∪ V_1))
   (E_1 ∪(LABS_1))]
 ; [(optimize-expression-inner Γ_1 (E_1 ⊆ E_2))
 ;  ((optimize-expression-inner Γ_1 E_1) ⊆ (optimize-expression-inner Γ_1 E_2))]
 ; [(optimize-expression-inner Γ_1 (E_1 ∪ E_2))
 ;  ((optimize-expression-inner Γ_1 E_1) ∪ (optimize-expression-inner Γ_1 E_2))]
  ; ORD-TRUE
  [(optimize-expression-inner Γ_1 (E_1 ⊆ E_2))
   (num 1)
   (side-condition
    (term (comparison-always-true?
           (lookupOrDefault Γ_1 E_1)
           (lookupOrDefault Γ_1 E_2))))]
  ; ORD-FALSE
  [(optimize-expression-inner Γ_1 (E_1 ⊆ E_2))
   (num 0)
   (side-condition
    (term (comparison-always-false?
           (lookupOrDefault Γ_1 E_1)
           (lookupOrDefault Γ_1 E_2))))]
    ; lookup vars first
  [(optimize-expression-inner _ E_1) E_1]
  )


(define firstTotal (lambda (ls)
                     (if (cons? ls) (first ls) '())))

; Optimizes the three commands IF, LET and ASSIGN and Labelexpressions.
(define-judgment-form VSIDO 
  #:mode     (⟹ I I I O)
  #:contract (⟹ Γ C : any)
  [
   --------------------------- R-ASSIGN
   (⟹ Γ_1 (V := E) : (V := (optimize-expression Γ_1 E)))]

  [(where N_1 (optimize-expression Γ_1 E_1))
   (side-condition (positive? (second (term N_1))))
   (⟹ Γ_1 C_1 : C_3)
   --------------------------- IF-TRUE
   (⟹ Γ_1 (if (E_1) {C_1} else {_}) : C_3)]

  [(where N_1 (optimize-expression Γ_1 E_1))
   (side-condition (not (positive? (second (term N_1)))))
   (⟹ Γ_1 C_2 : C_3)
   --------------------------- IF-FALSE
   (⟹ Γ_1 (if (E_1) {_} else {C_2}) : C_3)]

  [
   (where E_2 (optimize-expression Γ_1 E_1))
   (isNotN E_2)
   (⟹ Γ_1 C_1 : C_3)
   (⟹ Γ_1 C_2 : C_4)
   --------------------------- IF-FALSE-UNOPTIMIZED
   (⟹ Γ_1 (if (E_1) {C_1} else {C_2}) : (if (E_2) {C_3} else {C_4}))]

  [(⟹ Γ_1 C_1 : C_3)
   (⟹ Γ_1 C_2 : C_4)
   --------------------------- R-SEQ
   (⟹ Γ_1 (C_1 then C_2) : (C_3 then C_4))]

  [(⟹ Γ_1 C_1 : C_2)
   --------------------------- R-LET
   (⟹ Γ_1 (let var X_1 := E_1 in C_1) : (let var X_1 := (optimize-expression Γ_1 E_1) in C_2))]

  [(⟹ Γ_1 C_1 : C_2)
   --------------------------- R-WHILE
   (⟹ Γ_1 (while (E_1) do {C_1}) : (while (E_1) do {C_2}))]

  [
   --------------------------- R-OUT
   (⟹ Γ_1 (out(P < E)) : (out(P < E)))]


  [ (⟹ _ skip : skip) ]
  [ (⟹ _ halt : halt) ]
  )

(define-judgment-form VSIDO 
  #:mode     (isNotN I)
  #:contract (isNotN E)
  [(isNotN (E + E))]
  [(isNotN (E ⊆ E))]
  [(isNotN (E ∪ E))]
  [(isNotN V)]
  [(isNotN T)])