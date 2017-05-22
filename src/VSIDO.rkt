#lang racket
(require redex)

(define-language VSIDO
  (M ::= C E μ) ; helper needed for the substitution function
  (C ::= (L := E)
     (out(P 🡐 E))
     (if (E) {C} else {C})
     (while (E) do {C})
     (let var X := E in C)
     (C then C))
  (E ::= N L X (E + E)) ; expression
  (N ::= (num number)) ; numbers
  (L ::= (loc number)) ; locations
  (P ::= (port number)) ; ports
  (X ::= string) ; vars
  (STORE-ELEM ::= (L N) (P (N ...))) ; helper sum for locations and ports
  (μ ::= (STORE-ELEM ...))) ; mapping for the locations and ports

(define-judgment-form VSIDO
  #:mode (⇓ I I I O)
  #:contract (⇓ μ C : μ)

  [; R-LET
   ---------------------------
   (⇓ μ_1 (L := E) : (extL μ_1 L E))]

  [ ; R-OUT
   ---------------------------
   (⇓ μ_1 (out(P 🡐 E)) : (extP μ_1 P (eval μ_1 E)))]
   
  [(⇓ μ_1 (subst C X E) : μ_2); R-LET
   ---------------------------
   (⇓ μ_1 (let var X := E in C) : μ_2)]
  
   [(⇓ μ_1 C_1 : μ_2) ; R-SEQ
   (⇓ μ_2 C_2 : μ_3)
   ---------------------------
   (⇓ μ_1 (C_1 then C_2) : μ_3)]
  
  [(evals-to-zero? μ_1 E_1) ; R-IF-FALSE
   (⇓ μ_1 C_2 : μ_2)
   ---------------------------
   (⇓ μ_1 (if (E_1) {C_1} else {C_2}) : μ_2)]

  [(evals-to-biggerzero? μ_1 E_1); R-IF-TRUE
   (⇓ μ_1 C_1 : μ_2)
   ---------------------------
   (⇓ μ_1 (if (E_1) {C_1} else {C_2}) : μ_2)]
  
  [(evals-to-biggerzero? μ_1 E_1); R-WHILE-TRUE
   (⇓ μ_1 C_1 : μ_2)
   ---------------------------
   (⇓ μ_1 (while (E_1) do {C_1}) : μ_2)]
  
  [(evals-to-zero? μ_1 E_1); R-WHILE-FALSE
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
  ; appends a number to a port's output. Examples:
  ; ([1 (2 3 4)] [2 (9 9 9)]) 2 7 => ([1 (2 3 4)] [2 (9 9 9 7)])
  ; ([1 (2 3 4)]) 2 7             => ([1 (2 3 4)] [2 (7)])
  extP : (any ...) P N -> (any ...)
  [(extP (any_0 ... (P (any_1 ... )) any_2 ...) P N)
   (any_0 ... (P (any_1 ... N)) any_2 ...)]
  [(extP (any_0 ...) P N)
   (any_0 ... (P (N)))])

(define-metafunction VSIDO
  extL : (any ...) L N -> (any ...)
  [(extL (any_0 ... (L N_0) any_1 ...) L N_1)
   (any_0 ... (L N_1) any_1 ...)]
  [(extL (any_0 ...) L N)
   (any_0 ... (L N))])

(define-metafunction VSIDO
  eval : (any ...) E -> N
  [(eval _ N) N]
  [(eval (any_0 ... (L N) any_1 ...) L) N]
  [(eval μ_1 (E_0 + E_1))
   (num ,(+ (second (term (eval μ_1 E_0))) (second (term (eval μ_1 E_1)))))])

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
  [(subst (C_1 then C_2) X E) 
   ((subst C_1 X E) then (subst C_2 X E))]
  [(subst X X E) E]
  [(subst any _ _) any])
