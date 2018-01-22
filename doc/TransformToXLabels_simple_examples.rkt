#lang racket

(require "../src/TransformToXLabels.rkt" redex "../src/Grammar.rkt")


(println "x-assign-dyn")
(judgment-holds
 (>
  ()
  ((assignTo ((1) (2))) (assignFrom ((4)(5))))
  ()
  (assignTo := assignFrom) : Γ C)
 C)

(println "x-out-dyn")
(judgment-holds
 (>
  (((port 1) ((7))))
  ((aPotentiallyTooPublicVariable ((7) (6))))
  ()
  (out ((port 1) < aPotentiallyTooPublicVariable)) : Γ C)
 C)

(println "x-out")
(judgment-holds
 (>
  (((port 1) ((4 5)(6))))
  ((aSurelySafeVariable ((4 5) (6))))
  ()
  (out ((port 1) < aSurelySafeVariable)) : Γ C)
 C)

(println "x-let. aVar should be safe to write to port 1.")
(judgment-holds
 (>
  (((port 1) ((4 5)(6))))
  ((anotherVar ((4 5) (6))))
  ()
  (let var  aVar := anotherVar in (out ((port 1) < aVar))) : Γ C)
 C)

(println "x-seq.")
(judgment-holds
 (>
  ()
  ((aVar ()) (highVar ((9))) (lowVar ((2))))
  ()
  ((aVar := highVar) then (aVar := lowVar)) : Γ C)
 C)

(println "x-if.")
(judgment-holds
 (>
  (((port 1) ((4 5)(6))))
  ((aVar ((5))) (highVar ((9))) (lowVar ((2))))
  ()
  (if (aVar) {(out ((port 1) < highVar))} else {(out ((port 1) < lowVar))}) : Γ C)
 C)