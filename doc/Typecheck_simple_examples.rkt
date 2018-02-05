#lang racket

(require redex "../src/Typecheck.rkt")

(println "out - simple - success")

(judgment-holds
 (▷
  (((port 1) ((1 2 3 10))))
  ((aVar ((6))))
  ((3))
  (out((port 1) < aVar))
  : Γ)
 Γ)

(println "out - simple - failure. An empty environment means that there is no environment in which this judgement holds.")

(judgment-holds
 (▷
  (((port 1) ((1  3 10))))
  ((aVar ((6))))
  ((3))
  (out((port 1) < aVar))
  : Γ)
 Γ)

(println "out - indirect - success")

(judgment-holds
 (▷
  (((port 1) ((1 2 6))))
  ((CONDI ((6))) (A ((1))) (B ((2))))
  ()
  (if(CONDI)
     {(let var y := (num 3) in (out((port 1) < A)))} else
     {(out((port 1) < B))})
  : Γ)
 Γ)

(println "out - indirect - failure. An empty environment means that there is no environment in which this judgement holds.")

(judgment-holds
 (▷
  (((port 1) ((1 2 6))))
  ((CONDI ((5))) (A ((1))) (B ((2))))
  ()
  (if(CONDI)
     {(() then (out((port 1) < A)))} else
     {(out((port 1) < B))})
  : Γ)
 Γ)

(define incrWhile
  (term (while ((num 1)) do {
    ((x := (x + (num -1))) then ((w := x) then (
                    (x := y) then (y := z))))})))

(judgment-holds (▷ () ((w ()) (x ()) (y ()) (z ((42)))) () ,incrWhile : Γ) Γ)

(test-equal
 (first (judgment-holds (▷ () ((w ()) (x ()) (y ()) (z ((42)))) () ,incrWhile : Γ) Γ))
 (term ((w ((42))) (x ((42))) (y ((42))) (z ((42))))))