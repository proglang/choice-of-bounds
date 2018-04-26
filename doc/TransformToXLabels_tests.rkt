#lang racket

(require "../src/TransformToXLabels.rkt" redex rackunit)

(test-equal
(judgment-holds
 (>
  (((port 1) ((1 4 5 6))))
  ((aSurelySafeVariable ((4 5) (6))))
  ((1))
  (out ((port 1) < aSurelySafeVariable)) : Γ C)
 C)
(term ((out ((port 1) < aSurelySafeVariable)))))

(test-equal
(judgment-holds
  (>
   (((port 1) ((7))))
   ((aPotentiallyTooSecretVariable ((4 5) (6))))
   ((2))
   (out ((port 1) < aPotentiallyTooSecretVariable)) : Γ C)
  C)
(term ((skip then (if (((pc ∪ *aPotentiallyTooSecretVariable) ⊆ ((7)))) ((out ((port 1) < aPotentiallyTooSecretVariable))) else (halt))))))

; A potentially secret value is written to an output port. This must trigger an dynamic check.
(test-equal
 (judgment-holds
  (>
   (((port 1) ((7))))
   ((aPotentiallyTooSecretVariable ((4 5) (6))))
   ((2))
   (out ((port 1) < aPotentiallyTooSecretVariable)) : Γ C)
  C)
 '((skip
    then
    (if (((pc ∪ *aPotentiallyTooSecretVariable) ⊆ ((7))))
        ((out ((port 1) < aPotentiallyTooSecretVariable)))
        else
        (halt)))))

(println "All tests ran successfully.")