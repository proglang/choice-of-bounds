#lang racket

(require redex "../src/Grammar.rkt" "../src/Interprete.rkt" rackunit)

(test-equal #t
 (judgment-holds
  (evals-to-biggerzero?
   ((pc ((1))))
   (pc ⊆ ((1 2 7))))))

(test-equal #f
 (judgment-holds
  (evals-to-biggerzero?
   ((pc ((1))))
   (pc ⊆ ((7))))))

(test-equal #t
 (judgment-holds
  (evals-to-biggerzero?
   ((pc ((1))))
   (num 2))))

(test-equal #f
 (judgment-holds
  (evals-to-biggerzero?
   ()
   (num 0))))

(test-equal #t
 (judgment-holds
  (evals-to-biggerzero?
   ((pc (num 3)))
   pc)))

(test-equal #f
 (judgment-holds
  (evals-to-biggerzero?
   ((pc (num 0)))
   pc)))

(println "All tests ran successfully.")