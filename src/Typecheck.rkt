#lang racket
(require redex "Grammar.rkt" rackunit)

(provide ▷ choice choiceEnv remassoc declassify ⊑ ≤ multiplication evalT)


(define-judgment-form VSIDO ; type judgements for commands
  #:mode (▷ I I I I I O)
  #:contract (▷ Σ Γ T M : Γ)
  [
   --------------------------- ASSIGN
   (▷ Σ Γ_1 T_π (L_1 := E_1) : (extL Γ_1 L_1 (multiplication T_π (evalT Γ_1 E_1))))]
  [(side-condition ; If the side condition does not apply, an error should be thrown! use where/error from a newer redex version for this
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
   (▷ Σ Γ_1 T_π (if (E_1) {C_1} else {C_2}) : (choiceEnv Γ_2 Γ_3))]
  
  [(▷ Σ Γ_1 T_π C_1 : Γ_2)
   (side-condition (not (equal? (list->set Γ_1) (list->set Γ_2))))
   (▷ Σ Γ_2 T_π C_1 : Γ_3)
   --------------------------- WHILE-REDU ; TODO: WHILE
   (▷ Σ Γ_1 T_π (while (E_1) do {C_1}) : Γ_3)]

  [
   --------------------------- WHILE-BASE ; TODO: WHILE
   (▷ _ Γ_1 _ (while (_) do {_}) : Γ_1)])


(define-metafunction VSIDO
  lookupPortT : Σ P -> T
  [(lookupPortT (any ... (P T) any ...) P) T])


(define-metafunction VSIDO
  substT : Γ X T -> Γ)

(define-metafunction VSIDO
  ⊑ : (LAB ...) (LAB ...) -> boolean
  [(⊑ (name lhs (LAB_0 ...)) (name rhs (LAB_1 ...)))
   ,(not
    (false?
     (for/and
         ([lhs-elem (term lhs)])
       (member lhs-elem (term rhs)))))])
(define-metafunction VSIDO
  ≤ : T T -> boolean
  [(≤ T_1 T_2)
   ,(for/and
        ([i (term T_1)])
        (for/or
            ([j (term T_2)])
            (term (⊑ ,i ,j))))])

(define-metafunction VSIDO
  choice : T T -> T
  [(choice T_1 T_2) ,(set-union (term T_1) (term T_2))])

(define-metafunction VSIDO
  choiceEnv : Γ Γ -> Γ
  [(choiceEnv (name left (_ ... (V_1 T_1) _ ...)) (name right (_ ... (V_1 T_1) _ ...)))
   ,(sort
     (append
      (list (term (V_1 T_1)))
      (term (choiceEnv
             ,(remassoc (term V_1) (term left))
             ,(remassoc (term V_1) (term right)))))
     (lambda (x y)
       (string<? (symbol->string (car x)) (symbol->string (car y)))))]
  [(choiceEnv (name left (_ ... (V_1 T_1) _ ...)) (name right (_ ... (V_1 T_2) _ ...)))
   ,(append
     (list (term (V_1 (choice T_1 T_2))))
     (term (choiceEnv
            ,(remassoc (term V_1) (term left))
            ,(remassoc (term V_1) (term right)))))]
  [(choiceEnv Γ_1 Γ_2)
   ,(append (term Γ_1) (term Γ_2))])

; product operator: Given COB type A and B, A * B is the set of the pairwise sum of the inner sets of A and B.
; [ (1 2)     ] [ (4 5) (6 7) ] -> [ (1 2 4 5) (1 2 6 7)                 ]
; [ (1 2) (3) ] [ (4 5) (6 7) ] -> [ (1 2 4 5) (1 2 6 7) (3 4 5) (3 6 7) ]
(define-metafunction VSIDO
  multiplication : T T -> T
  [(multiplication T_1 ()) T_1]
  [(multiplication () T_2) T_2]
  [(multiplication T_1 T_2)
   ,(map flatten (cartesian-product (term T_1) (term T_2)))])

(define-metafunction VSIDO ; declassifies a COB type
  declassify : T LAB LAB LAB -> T
  [(declassify T LAB_A LAB_B LAB_C)
   ,(map
    (lambda (i) (term (declassify-on-sets ,i LAB_A LAB_B LAB_C)))
    (term T))])

(define-metafunction VSIDO ; declassifies a set of labels (equivalenty, a inner set of a COB type)
  declassify-on-sets : (LAB ...) LAB LAB LAB -> (LAB ...)
  [(declassify-on-sets (name inner-set (LAB ...)) LAB_A LAB_B LAB_C)
   ,(map
    (lambda (x)
     (cond
       ; Instead of looking at the ordering ⊑, simply use the ordering < of the natural numbers for now.
        [(and (< x (term LAB_A)) (< x (term LAB_B)) (not (< x (term LAB_C)))) (term LAB_C)]
        [else x]))
    (term inner-set))])

;(term (declassify-on-sets
;       (1 3 5)
;       4
;       4
;       2))
;(term (declassify
;       ((1 3 5))
;       4
;       4
;       2))


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
   (,(set-union
      (first (term (evalT Γ_1 E_1)))
      (first (term (evalT Γ_1 E_2)))))])

(define-metafunction VSIDO
  ; updates the location mapping. Locations can be mapped to expressions or types.
  extL : (any ...) L any -> (any ...)
  [(extL (any_0 ... (L any)   any_1 ...) L any_2)
         (any_0 ... (L any_2) any_1 ...)]
  [(extL (any_0 ...) L any_2)
         (any_0 ... (L any_2))])

(define (remassoc v lst)
  (remove (assoc v lst) lst))


; TODO WHILE-METAFUNCTION
; TODO Keine Duplicates!
; TODO indirekt flow

;(redex-match? VSIDO C (term (while ((num 1)) do { ((loc 1) := ((num 42) :: ((2)))) })))
;
;(judgment-holds
; (▷ () (((loc 1) ((4)))) ((9)) (while ((num 1)) do { ((loc 1) := ((num 42) :: ((2)))) }) : Γ)
; Γ)

