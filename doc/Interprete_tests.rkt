#lang racket

(require redex "../src/Grammar.rkt" "../src/Interprete.rkt")

(judgment-holds (evals-to-biggerzero?
                 ((pc ((1))))
                 (pc ⊆ ((1 2 7)))))
(judgment-holds (evals-to-biggerzero?
                 ((pc ((1))))
                 (pc ⊆ ((7)))))

(judgment-holds (evals-to-biggerzero?
                 ((pc ((1))))
                 (num 2)))
(judgment-holds (evals-to-biggerzero?
                 ()
                 (num 0)))
(judgment-holds (evals-to-biggerzero?
                 ((pc (num 3)))
                 pc))
(judgment-holds (evals-to-biggerzero?
                 ((pc (num 0)))
                 pc))