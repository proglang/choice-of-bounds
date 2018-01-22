#lang racket

(require redex "../src/Grammar.rkt" "../src/Interprete.rkt" "../src/TransformToXLables.rkt"
         "../src/Optimize.rkt")

(println "indirect flow where dynamic check is necessary. ")
(define indFlowCmd (judgment-holds
 (>
  (((port 1) ((5))))
  ((pc ())(aVar ((2))) (password ((7)(5))))
  ()
  ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))})) : Γ C)
 C))
(define indFlowEnv (judgment-holds
 (>
  (((port 1) ((5))))
  ((pc ())(aVar ((2))) (password ((7)(5))))
  ()
  ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))})) : Γ C)
 Γ))
indFlowEnv
indFlowCmd
(judgment-holds (⟹
                 ,(first indFlowEnv)
                 ,(first indFlowCmd) : C ) C)
(println "indirect flow where dynamic check not necessary because output will always succeed. ")
(define indFlowCmdSuc (judgment-holds
 (>
  (((port 1) ((5))))
  ((pc ())(aVar ((2))) (password ((5))))
  ()
  ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))})) : Γ C)
 C))
(define indFlowEnvSuc (judgment-holds
 (>
  (((port 1) ((5))))
  ((pc ())(aVar ((2))) (password ((5))))
  ()
  ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))})) : Γ C)
 Γ))
;indFlowEnvSuc
;indFlowCmdSuc
(judgment-holds (⟹
                 ,(first indFlowEnvSuc)
                 ,(first indFlowCmdSuc) : C ) C)

(println "indirect flow where dynamic check not necessary because output will always fail. ")
(define indFlowCmdFail (judgment-holds
 (>
  (((port 1) ((5))))
  ((pc ())(aVar ((2))) (password ((7))))
  ()
  ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))})) : Γ C)
 C))
(define indFlowEnvFail (judgment-holds
 (>
  (((port 1) ((5))))
  ((pc ())(aVar ((2))) (password ((7))))
  ()
  ((aVar := password) then (if (aVar)
                                {(out ((port 1) < (num 42)))} else
                                {(out ((port 1) < (num -1)))})) : Γ C)
 Γ))
;indFlowEnvFail
indFlowCmdFail
(judgment-holds (⟹
                 ,(first indFlowEnvFail)
                 ,(first indFlowCmdFail) : C ) C)

(term (optimize-expression ((pc ()) (*aVar ((7)))) (pc ⊆ *aVar)))
(term (optimize-expression ((pc ((7))) (*aVar ((7)))) (pc ⊆ ((7)))))
(term (optimize-expression ((pc ((7))) (*aVar ((7)))) (((7)) ⊆ ((7)))))


