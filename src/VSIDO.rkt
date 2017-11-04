#lang racket
(require redex)

(provide VSIDO ⇓ fresh-location extμ subst)

(define-language VSIDO
  (M ::= C E μ) ; helper needed for the substitution function
  (V ::= X L) ; variables
  (C ::= (V := E)
     (out(P 🡐 E))
     (if (E) {C} else {C})
     (while (E) do {C})
     (let var X := E in C)
     (C then C))
  (E ::= N V (E + E)) ; expression
  (N ::= (num number)) ; numbers
  (L ::= (loc number)) ; locations
  (P ::= (port number)) ; ports
  (X ::= variable-not-otherwise-mentioned) ; literals
  (STORE-ELEM ::= (L N) (P (N ...))) ; helper sum for locations and ports
  (μ ::= (STORE-ELEM ...))) ; mapping for the locations and ports

(define-judgment-form VSIDO
  #:mode (⇓ I I I O)
  #:contract (⇓ μ C : μ)

  [
   --------------------------- R-ASSIGN
   (⇓ μ_1 (L := E) : (extμ μ_1 L (eval μ_1 E)))]

  [
   --------------------------- R-OUT
   (⇓ μ_1 (out(P 🡐 E)) : (extμ μ_1 P (eval μ_1 E)))]
   
  [(where L_1 (fresh-location μ_1))
   (where N_1 (eval μ_1 E_1))
   (⇓ (extμ μ_1 L_1 N_1) (subst C_1 X_1 L_1) : μ_2)
   --------------------------- R-LET
   (⇓ μ_1 (let var X_1 := E_1 in C_1) : μ_2)]
  
   [(⇓ μ_1 C_1 : μ_2)
   (⇓ μ_2 C_2 : μ_3)
   --------------------------- R-SEQ
   (⇓ μ_1 (C_1 then C_2) : μ_3)]
  
  [(evals-to-zero? μ_1 E_1)
   (⇓ μ_1 C_2 : μ_2)
   --------------------------- R-IF-FALSE
   (⇓ μ_1 (if (E_1) {C_1} else {C_2}) : μ_2)]

  [(evals-to-biggerzero? μ_1 E_1)
   (⇓ μ_1 C_1 : μ_2)
   --------------------------- R-IF-TRUE
   (⇓ μ_1 (if (E_1) {C_1} else {C_2}) : μ_2)]
  
  [(evals-to-biggerzero? μ_1 E_1)
   (⇓ μ_1 C_1 : μ_2)
   (⇓ μ_2 (while (E_1) do {C_1}) : μ_3)
   --------------------------- R-WHILE-TRUE
   (⇓ μ_1 (while (E_1) do {C_1}) : μ_3)]
  
  [(evals-to-zero? μ_1 E_1)
   --------------------------- R-WHILE-FALSE
   (⇓ μ_1 (while (E_1) do {C_1}) : μ_1)])


(define-judgment-form VSIDO
  #:mode (evals-to-zero? I I)
  #:contract (evals-to-zero? μ E)
  [(evals-to-zero? _ (num (side-condition (name N_1 number) (not (positive? (term N_1))))))]
  [(evals-to-zero? (_ ... (L (num (side-condition (name N_1 number) (not (positive? (term N_1)))))) _ ... ) L)])
(define-judgment-form VSIDO
  #:mode (evals-to-biggerzero? I I)
  #:contract (evals-to-biggerzero? μ E)
  [(evals-to-biggerzero? _ (side-condition (name N_1 N) (positive? (second (term N_1)))))]
  [(evals-to-biggerzero? (_ ... (L (num (side-condition (name N_1 number) (positive? (term N_1))))) _ ... ) L)])

(define-metafunction VSIDO
  ; Updates the environment μ.
  ; If a port should be updated, the new value either gets appended to the existing port or a new port is introduced into the environment.
  ; ([1 (2 3 4)] [2 (9 9 9)]) 2 7 => ([1 (2 3 4)] [2 (9 9 9 7)])
  ; ([1 (2 3 4)]            ) 2 7 => ([1 (2 3 4)] [2 (      7)])
  ; If a location should be updated, either its old value is replaced if already existent or it gets introduced with its new value.
  ; ([1 42]      [2     111]) 2 7 => ([1      42] [2         7])
  ; ([1 42]                 ) 2 7 => ([1      42] [2         7])
  extμ : μ any N -> μ
  [(extμ (any_0 ... (P (any_1 ...  )) any_2 ...) P N)
         (any_0 ... (P (any_1 ... N)) any_2 ...)]
  [(extμ (any_0 ...) P N)
         (any_0 ... (P (N)))]
  [(extμ (any_0 ... (L any) any_1 ...) L N)
         (any_0 ... (L N) any_1 ...)]
  [(extμ (any_0 ...) L N)
         (any_0 ... (L N))])

(define-metafunction VSIDO
  eval : (any ...) E -> N
  [(eval _ N) N]
  [(eval (any_0 ... (L N) any_1 ...) L) N]
  [(eval μ_1 (E_0 + E_1))
   (num ,(+ (second (term (eval μ_1 E_0))) (second (term (eval μ_1 E_1)))))])

(define-metafunction VSIDO
  subst : M X L -> M
  [(subst (if (       E_1       ) {       C_1       } else {       C_2       }) X L_2)
          (if ((subst E_1 X L_2)) {(subst C_1 X L_2)} else {(subst C_2 X L_2)})       ]
  [(subst (while (       E_1         ) do {       C         }) X_1 L_2)
          (while ((subst E_1 X_1 L_2)) do {(subst C X_1 L_2)}         )]
  [(subst (let var X_1 := E_1 in C) X_1 L_2) ; overshadow
          (let var X_1 := E_1 in C)]
  [(subst (let var X_1 := E_1 in C) X_2 L_2) 
          (let var X_1 := (subst E_1 X_2 L_2) in (subst C X_2 L_2))]
  [(subst (out(P 🡐        E_1      )) X L_2) 
          (out(P 🡐 (subst E_1 X L_2))      )]
  [(subst (       C_1      then        C_2    ) X L) 
          ((subst C_1 X L) then (subst C_2 X L)    )]
  [(subst (       E_1      +        E_2) X L)
          ((subst E_1 X L) + (subst E_2  X L))]
  [(subst (       V      :=        E)    X L) 
          ((subst V X L) := (subst E X L)   )]
  [(subst X X L) L]
  [(subst any _ _) any])

(define-metafunction VSIDO
  fresh-location : μ -> any
  [(fresh-location ()) (loc 1)]
  [(fresh-location μ_1)
   (loc ,(+
         1
         (apply max
                (map (lambda (t)
                       (cond
                         [(redex-match VSIDO L (first t)) (second (first t))]
                         [else 0]))
                     (term μ_1)))))])

