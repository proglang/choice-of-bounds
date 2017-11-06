#lang racket
(require redex "VSIDO.rkt" rackunit)

(provide VSIDOT ▷ choice choiceEnv remassoc declassify ⊑ ≤)

(define-extended-language VSIDOT VSIDO
  (T ::= ((LAB ...) ...))
  (LAB ::= number)
  (E ::= ....
     (E :: T)
     (dcl E LAB LAB LAB))
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
   (▷ Σ Γ_1 T_π (if (E_1) {C_1} else {C_2}) : (choiceEnv Γ_2 Γ_3))]
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
  ⊑ : (LAB ...) (LAB ...) -> boolean
  [(⊑ (name lhs (LAB_0 ...)) (name rhs (LAB_1 ...)))
   ,(not
    (false?
     (for/and
         ([lhs-elem (term lhs)])
       (member lhs-elem (term rhs)))))])
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

(define-metafunction VSIDOT
  choiceEnv : Γ Γ -> Γ
  [(choiceEnv (name left (_ ... (V_1 T_1) _ ...)_left) (name right (_ ... (V_1 T_2) _ ...)))
   ,(append
     (list (term (V_1 (choice T_1 T_2))))
     (term (choiceEnv
            ,(remassoc (term V_1) (term left))
            ,(remassoc (term V_1) (term right)))))]
  [(choiceEnv Γ_1 Γ_2)
   ,(append (term Γ_1) (term Γ_2))])

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
  declassify : (LAB ...) (LAB ...) (LAB ...) (LAB ...) -> T
  [(declassify (name to-map (LAB ...)) (name A (LAB ...)) (name B (LAB ...)) (name C (LAB ...)))
   (cond
        [(and (⊑ to-map A) (⊑ to-map B) (not (⊑ to-map C))) C]
        [else to-map])])

(define-metafunction VSIDOT
  evalT : Γ E -> T
  [(evalT _ N)     (())]
  [(evalT _ (_ :: T)) T]
  [(evalT Γ_1 X_1) ,(second (assoc (term X_1) (term Γ_1)))]
  [(evalT Γ_1 (dcl E_1 LAB_A LAB_B LAB_C))
   (map
    (lambda (lables)
      (declassify lables LAB_A LAB_B LAB_C))
    (evalT Γ_1 E_1))]
  [(evalT Γ_1 (M_1 + M_2))
   (multiplication
    (evalT Γ_1 M_1)
    (evalT Γ_1 M_2))])

(define-metafunction VSIDOT
  ; updates the location mapping. Locations can be mapped to expressions or types.
  extL : (any ...) L any -> (any ...)
  [(extL (any_0 ... (L any) any_1 ...) L any_2)
         (any_0 ... (L any_2) any_1 ...)]
  [(extL (any_0 ...) L any_2)
         (any_0 ... (L any_2))])

(define (remassoc v lst)
  (remove (assoc v lst) lst))

