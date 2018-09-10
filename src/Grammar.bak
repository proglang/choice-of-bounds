#lang racket
(require redex)

(provide VSIDO lookup ext lookupOrDefault free secondTotal assocTotal)

(define-language VSIDO
  (V ::= X L) ; Variables
  (X ::= variable-not-otherwise-mentioned) ; Variable names
  (L ::= (loc number)) ; Locations
  (N ::= (num number)) ; Numbers
  (P ::= (port number)) ; Ports
  (C ::= (V := E)
     (out(P < E))
     (if (E) {C} else {C})
     (while (E) do {C})
     (let var X := E in C)
     (C then C)
     skip
     halt)
  (E ::= N
     NonNumericalExpression) ; A helper expression for pattern matching.
  (NonNumericalExpression ::=
                          V (E + E) ; Expression relevant to interpreting the program.
                          (dcl E LAB LAB LAB) ; A declassification.
                          TR ; A least upper bound of a COB type's inner sets.
                          (E ∪ E) ; Runtime union of two sets of lables.
                          (E ⊆ E)) ; Runtime check whether one set of labels is contained in the other.
  (STORE-ELEM ::= (V TR) (V N) (P (N ...))) ; Helper sum for locations and ports.
  (μ ::= (STORE-ELEM ...)) ; the evaluation environment
  (LAB ::= number) ; A label. For now, simply a number.
  (LABS ::= (LAB LAB ...)) ; A nonempty set of labels. Used both as inner set of a COB or a lub of a COB.
  (T ::= (LABS ...)) ; COB type. Contains zero or more inner sets consisting of labels.
  (TR := (LABS) ()) ; Reified COB type - contains on inner set at most because at runtime there is only one control flow to represent.
  (Γ ::= ((V T) ...)) ; Variable type environment.
  (Σ ::= ((P T) ...)) ; Port type environment.
  (VOrT := T V) ; Helper for easier pattern matching in optimizing. Semantically, runtime types or reprentations of it.
  (M ::= C E μ)) ; Helper needed for the substitution function
  
; Updates an environment.
; If a port should be updated, the new value either gets appended to the existing port or a new port is introduced into the environment.
; ([1 (2 3 4)] [2 (9 9 9)]) 2 7 => ([1 (2 3 4)] [2 (9 9 9 7)])
; ([1 (2 3 4)]            ) 2 7 => ([1 (2 3 4)] [2 (      7)])
; If a variable should be updated, either its old value is replaced if already existent or it gets introduced with its new value.
; ([1 42]      [2     111]) 2 7 => ([1      42] [2         7])
; ([1 42]                 ) 2 7 => ([1      42] [2         7])
(define-metafunction VSIDO
  ext : (any ...) any any -> (any ...)
  [(ext (any_0 ... (P (any_1 ...  )) any_2 ...) P N)
        (any_0 ... (P (any_1 ... N)) any_2 ...)]
  [(ext (any_0 ...) P N)
        (any_0 ... (P (N)))]
  [(ext (any_0 ... (any_k _) any_1 ...) any_k any_v)
        (any_0 ... (any_k any_v) any_1 ...)]
  [(ext (any_0 ...) any_k any_v)
        (any_0 ... (any_k any_v))])

; Lookup a value in an environment.
(define-metafunction VSIDO
  lookup : (any ...) any -> any
  [(lookup (_ ... (any_k any_v) _ ...) any_k) any_v])

; Lookup a value in an environment. If the value is not contained in the environment, return the value to lookup.
(define-metafunction VSIDO
  lookupOrDefault : (any ...) any -> any
  [(lookupOrDefault (_ ... (any_k any_v) _ ...) any_k) any_v]
  [(lookupOrDefault _ any_1) any_1])

; Find all variables of a term that are not bound by a let. 
(define-metafunction VSIDO
  free : M -> (X ... )
  [(free X_1) (X_1)]
  [(free (E_1 + E_2)) ,(set-union (term (free E_1)) (term (free E_2)))]
  [(free (E_1 ∪ E_2)) ,(set-union (term (free E_1)) (term (free E_2)))]
  [(free (E_1 ⊆ E_2)) ,(set-union (term (free E_1)) (term (free E_2)))]
  [(free (dcl E_1 _ _ _)) (free E_1)]
  [(free (V_1 := E_1)) ,(set-union (term (free V_1)) (term (free E_1)))]
  [(free (out(_ < E_1))) (free E_1)]
  [(free (if (E_1) {C_1} else {C_2})) ,(set-union (term (free E_1)) (term (free C_1)) (term (free C_2)))]
  [(free (while (E_1) do {C_1})) ,(set-union (term (free E_1)) (term (free C_1)))]
  [(free (C_1 then C_2)) ,(set-union (term (free C_1)) (term (free C_2)))]
  [(free (let var X_1 := E_1 in C_1))
   ,(set-union
    (term (free E_1))
    (set-remove (term (free C_1)) (term (X_1))))]
  [(free _) ()])

; Return the first element of the list if is an nonempty list. A new empty list otherwise.
(define firstTotal (lambda (ls)
                     (if (cons? ls) (first ls) '())))

; Return the second element of the list if is an nonempty list. A new empty list otherwise.
(define secondTotal
  (lambda (ls)
    (if (null? ls) '() (second ls))))

; Return a value v of the association with key k. A new empty list otherwise.
(define assocTotal
  (lambda (k ls)
    (let ([v (assoc k ls)])
      (if (not v) '() v))))