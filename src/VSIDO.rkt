#lang racket
(require redex)

(define-language VSIDO
  (M ::= C E)
  (C ::= (out(P 🡐 E))
     (if (E) {C} else {C})
     (while (E) do {C})
     (let var X := E in C))
  (E ::= N L X) ; expression
  (N ::= (num number)) ; numbers
  (L ::= (loc number)) ; locations
  (P ::= (port number)) ; ports
  (X ::= string) ; vars
  (STORE-ELEM ::= (L N) (P (N ...))) ; helper sum for locations and ports
  (μ ::= (STORE-ELEM ...)))

(define-judgment-form VSIDO
  #:mode (⇓ I I I O)
  #:contract (⇓ μ C : μ)

  [ ; R-OUT
   ---------------------------
   (⇓ μ (out(P 🡐 E)) : (extP μ P E))]
   
  [ ; R-LET
   (⇓ μ_1 (subst C X E) : μ_2)
   ---------------------------
   (⇓ μ_1 (let var X := E in C) : μ_2)]
   
  [; R-IF-FALSE
   (evals-to-zero? μ_1 E_1)
   (⇓ μ_1 C_2 : μ_2)
   ---------------------------
   (⇓ μ_1 (if (E_1) {C_1} else {C_2}) : μ_2)]

  [; R-IF-TRUE
   (evals-to-biggerzero? μ_1 E_1)
   (⇓ μ_1 C_1 : μ_2)
   ---------------------------
   (⇓ μ_1 (if (E_1) {C_1} else {C_2}) : μ_2)]
  
  [; R-WHILE-TRUE
   (evals-to-biggerzero? μ_1 E_1)
   (⇓ μ_1 C_1 : μ_2)
   ---------------------------
   (⇓ μ_1 (while (E_1) do {C_1}) : μ_2)]
  
  [; R-WHILE-FALSE
   (evals-to-zero? μ_1 E_1)
   ---------------------------
   (⇓ μ_1 (while (E_1) do {C_1}) : μ_1)])


(define-judgment-form VSIDO
  #:mode (evals-to-zero? I I)
  #:contract (evals-to-zero? ((any any) ...) any)
  [(evals-to-zero? _ (num 0))]
  [(evals-to-zero? (_ ... (L 0) _ ... ) L)])
(define-judgment-form VSIDO
  #:mode (evals-to-biggerzero? I I)
  #:contract (evals-to-biggerzero? ((any any) ...) any)
  [(evals-to-biggerzero? _ (side-condition (name N_1 number) (not (zero? (term N_1)))))]
  [(evals-to-biggerzero? (_ ... (L (num (side-condition (name N_1 number) (not (zero? (term N_1)))))) _ ... ) L)])

(define-metafunction VSIDO
  ; appends a number to a port's output.
  ; example: ([1 (2 3 4)] [2 (9 9 9)]) 2 7 => ([1 (2 3 4)] [2 (9 9 9 7)])
  ; example: ([1 (2 3 4)]) 2 7 => ([1 (2 3 4)] [2 (7)])
  extP : (any ...) P N -> (any ...)
  [(extP (any_0 ... (P (any_1 ... )) any_2 ...) P N)
   (any_0 ... (P (any_1 ... N)) any_2 ...)]
  [(extP (any_0 ...) P N)
   (any_0 ... (P (N)))])

(define-metafunction VSIDO
  subst : M X E -> M
  [(subst (if (E_1) {C_1} else {C_2}) X E_2)
   (if (subst E_1 X E_2) {(subst C_1 X E_2)} else {(subst C_2 X E_2)})]
  [(subst (while (E_1) do {C} X E_2))
   (while (E_1) do {(subst C X E_2)})]
  [(subst (let var X := E_1 in C) X E_2) 
   (let var X := E_1 in C)]
  [(subst (let var X_1 := E_1 in C) X_2 E_2) 
   (let var X_1 := (subst E_1 X_2 E_2) in (subst C X_2 E_2))]
  [(subst (out(P 🡐 E_1)) X E_2) 
   (out(P 🡐 (subst E_1 X E_2)))]
  [(subst X X E) E]
  [(subst any _ _) any])

(define-language REDEX) ;; the language of generic functions
(define-metafunction REDEX
  ext1 : ((any any) ...) (any any) -> ((any any) ...)
  [(ext1 (any_0 ... (any_k any_v0) any_1 ...) (any_k any_v1))
   (any_0 ... (any_k any_v1) any_1 ...)]
  [(ext1 (any_0 ...) (any_k any_v1))
   ((any_k any_v1) any_0 ...)])
(define-metafunction REDEX
  ext : ((any any) ...) (any any) ... -> ((any any) ...)
  [(ext any) any]
  [(ext any any_0 any_1 ...)
   (ext1 (ext any any_1 ...) any_0)])
