#lang racket
(require redex)

(provide VSIDO lookup ext lookupOrDefault)

(define-language VSIDO
  (M ::= C E μ) ; helper needed for the substitution function
  (V ::= X L) ; variables
  (C ::= (V := E)
     (out(P < E))
     (if (E) {C} else {C})
     (while (E) do {C})
     (let var X := E in C)
     (C then C)
     skip
     halt)
  (E ::= N V (E + E) ; Expression relevant to interpreting the program.
     (dcl E LAB LAB LAB) ; A declassification.
     ;LABS ; A lub of a COB.
     T ; Due to the powerset interpretation, this looks like a type, semantically this is a lub of a COB type's inner sets.
     (E ∪ E) ; Runtime union of two sets of lables.
     (E ⊆ E)) ; Runtime check whether one set of labels is contained in the other.)
  (N ::= (num number)) ; numbers
  (L ::= (loc number)) ; locations
  (P ::= (port number)) ; ports
  (X ::= variable-not-otherwise-mentioned) ; literals
  (STORE-ELEM ::= (V TR) (V N) (P (N ...))) ; helper sum for locations and ports
  (μ ::= (STORE-ELEM ...))
  ; these are needed for type checking
  (LAB ::= number) ; A label. For now, simply a number.
  (LABS ::= (LAB LAB ...)) ; A nonempty set of labels. Used both as inner set of a COB or a lub of a COB.
  (T ::= (LABS ...)) ; COB type. Contains zero or more inner sets consisting of labels.
  (TR := (LABS) ()) ; Reified COB type - contains on inner set at most because at runtime there only one control flow to represent.
  (π := T) ; context type.
  (Γ ::= ((V T) ...)) ; Variable type environment.
  (Σ ::= ((P T) ...)) ; Port type environment.
  (Evaled ::= N T) ; Sum type, for a finer-grained eval metafunction.
  )

(define-metafunction VSIDO
  ; Updates the environment μ.
  ; If a port should be updated, the new value either gets appended to the existing port or a new port is introduced into the environment.
  ; ([1 (2 3 4)] [2 (9 9 9)]) 2 7 => ([1 (2 3 4)] [2 (9 9 9 7)])
  ; ([1 (2 3 4)]            ) 2 7 => ([1 (2 3 4)] [2 (      7)])
  ; If a location should be updated, either its old value is replaced if already existent or it gets introduced with its new value.
  ; ([1 42]      [2     111]) 2 7 => ([1      42] [2         7])
  ; ([1 42]                 ) 2 7 => ([1      42] [2         7])
  ext : (any ...) any any -> (any ...)
  [(ext (any_0 ... (P (any_1 ...  )) any_2 ...) P N)
        (any_0 ... (P (any_1 ... N)) any_2 ...)]
  [(ext (any_0 ...) P N)
        (any_0 ... (P (N)))]
  [(ext (any_0 ... (any_k any) any_1 ...) any_k any_v)
        (any_0 ... (any_k any_v) any_1 ...)]
  [(ext (any_0 ...) any_k any_v)
        (any_0 ... (any_k any_v))])

(define-metafunction VSIDO
  lookup : (any ...) any -> any
  [(lookup (_ ... (any_k any_v) _ ...) any_k) any_v])

(define-metafunction VSIDO
  lookupOrDefault : (any ...) any -> any
  [(lookupOrDefault (_ ... (any_k any_v) _ ...) any_k) any_v]
  [(lookupOrDefault _ any_1) any_1])
