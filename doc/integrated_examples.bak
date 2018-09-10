#lang racket

(require redex "../src/TransformToXLabels.rkt" "../src/Optimize.rkt")

; The program that will be used in every example.
(define program (term ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))}))))

; The static type of the port.
(define portEnv (term (((port 1) ((3 5))))))


;(println "indirect flow where dynamic check is necessary without optimization ")
;(define indFlowCmd (judgment-holds (> ,portEnv ,indFlowTypes () ,program : Γ C) C))

(println "Dynamic check is necessary.")

(define indFlowTypes (term ((pc ())(*pc ())(aVar ((3))) (*aVar ((3))) (password ((3)(5)(2))) (*password ((3)(5)(2))))))

(term (optimize
       ,(first (judgment-holds (> ,portEnv ,indFlowTypes () ,program : Γ C) C))
       ,(first (judgment-holds (> ,portEnv ,indFlowTypes () ,program : Γ C) Γ))))

(println "Dynamic check is not necessary because output will always succeed. ")

(define indFlowSucEnv (term ((pc ())(*pc ())(aVar ((3))) (*aVar ((3))) (password ((3) (5))) (*password ((3) (5))))) )

(term (optimize
       ,(first (judgment-holds (> ,portEnv ,indFlowSucEnv () ,program : Γ C) C))
       ,(first (judgment-holds (> ,portEnv ,indFlowSucEnv () ,program : Γ C) Γ))))

(println "Dynamic check is not necessary because output will always fail. ")

(define indFlowFailEnv (term ((pc ())(*pc ())(aVar ((2))) (*aVar ((2))) (password ((2) (4))) (*password ((2) (4))))) )

(term (optimize
       ,(first (judgment-holds (> ,portEnv ,indFlowFailEnv () ,program : Γ C) C))
       ,(first (judgment-holds (> ,portEnv ,indFlowFailEnv () ,program : Γ C) Γ))))

