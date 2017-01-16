#lang racket
(require redex)

(define-language REDEX) ;; the language of generic functions
(define-judgment-form REDEX
    #:mode (lookup I I O)
    #:contract (lookup ((any any) ...) any any)
    [(lookup (_ ... (any any_0) _ ...) any any_0)])
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

(define-language VSIDO
    (W ::= V ;; W stands for word
           (out (X V))
           (ite V W W)
           (let var X := V in W))
    (P ::= string) ;; port
    (V ::= N X) ;; value
    (N ::= number)
    (X ::= string)
    (μ ::= ((X V) ...)))

(define-judgment-form VSIDO
    #:mode (⇓ I I I O)
    #:contract (⇓ μ W : μ)

   [
     ---------------------------
     (⇓ μ_1 (let var X := V in W) : (ext μ_1 (X V)))]
  
    [(lookup μ_1 V_1 V_2) ;; μ_1 may not contain V_1 ;; TODO: exception?
     (where V_3, (if (number? (term V_1))(term V_1)(term V_2)))
     (where W ,(if (zero? (term V_3)) (term W_2) (term W_1)))
     (⇓ μ_1 W : μ_2)
     ---------------------------
     (⇓ μ_1 (ite V_1 W_1 W_2) : μ_2)])