#lang racket

(require redex  "../src/Interprete.rkt" "../src/Grammar.rkt" "../src/Optimize.rkt")

(println "IF-TRUE")
(judgment-holds (⟹ ((pc ((1 3))) (*x ((1 5))))
                   (if (((pc ∪ *x) ⊆ ((1 3 5)))) (skip) else (halt))
                   : C ) C)
(println "IF-FALSE")
(judgment-holds (⟹ ((pc ((1 3))) (*x ((1 5))))
                   (if ((((3 5 7))  ⊆ (pc ∪ *x))) (skip) else (halt))
                   : C ) C)

