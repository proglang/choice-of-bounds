#lang racket
(require redex "Grammar.rkt" "Typecheck.rkt")

(provide ⇓ fresh-location evals-to-biggerzero? evals-to-zero? ext subst lookup eval booleanToN)

(define-judgment-form VSIDO
  #:mode (⇓ I I I O)
  #:contract (⇓ μ C : μ)

  [
   --------------------------- R-ASSIGN
   (⇓ μ_1 (L := E) : (ext μ_1 L (eval μ_1 E)))]

  [
   --------------------------- R-OUT
   (⇓ μ_1 (out(P < E)) : (ext μ_1 P (eval μ_1 E)))]
   
  [(where L_1 (fresh-location μ_1))
   (where N_1 (eval μ_1 E_1))
   (⇓ (ext μ_1 L_1 N_1) (subst C_1 X_1 L_1) : μ_2)
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
  [(side-condition (not (≤ (eval μ_1 E_1) (eval μ_1 E_2))))
   ---------------------------
   (evals-to-zero? μ_1 (E_1 ⊆ E_2) )]
  [(evals-to-zero? _ (side-condition (name N_1 N) (not (positive? (second (term N_1))))))]
  [(evals-to-zero? (_ ... (V (side-condition (name N_1 N) (not (positive? (second (term N_1)))))) _ ... ) V)]
)
(define-judgment-form VSIDO
  #:mode (evals-to-biggerzero? I I)
  #:contract (evals-to-biggerzero? μ E)
  [(side-condition (≤ (eval μ_1 E_1) (eval μ_1 E_2)))
   ---------------------------
   (evals-to-biggerzero? μ_1 (E_1 ⊆ E_2) )]
  [(evals-to-biggerzero? _ (side-condition (name N_1 N) (positive? (second (term N_1)))))]
  [(evals-to-biggerzero? (_ ... (V (side-condition (name N_1 N) (positive? (second (term N_1))))) _ ... ) V)]
  )

;(define-metafunction VSIDO
;  eval : μ E -> Evaled
;  [(eval _ N) N]
;  [(eval (_ ... (V N) _ ...) V) N]
;  [(eval μ_1 (E_0 + E_1))
;   (num ,(+ (second (term (eval μ_1 E_0))) (second (term (eval μ_1 E_1)))))])

;(define firstTotal (lambda (ls)
;                     (if (cons? ls) (first ls) '())))

(define-metafunction VSIDO
  eval : μ E -> any
  [(eval _ N) N]
  [(eval (_ ... (V N) _ ...) V) N]
  [(eval μ_1 (E_0 + E_1))
   (num ,(+
          (second (term (eval μ_1 E_0)))
          (second (term (eval μ_1 E_1)))))]
  ; From here on, these are the evaluation rules for runtime types. In particular, the types do not contain more than one inner set. 
  [(eval _ T) T]
  [(eval (_ ... (V T) _ ...) V) T]
  
  [(eval μ_1 (E_1 ∪ E_2)) (,(set-union
                             (first (term (eval μ_1 E_1)))
                             (first (term (eval μ_1 E_2)))))]
  [(eval _ any) any]) ; TODO is this a good idea?

(define-metafunction VSIDO
  booleanToN : boolean -> N
  [(booleanToN #t) (num 1)]
  [(booleanToN #f) (num 0)])

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
  [(subst (out(P <        E_1      )) X L_2) 
          (out(P < (subst E_1 X L_2))      )]
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

