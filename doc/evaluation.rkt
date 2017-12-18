#lang racket
(require redex "../src/VSIDO.rkt")

#|
These examples demonstrate the semantics of VSIDO. To view results, simply execute this file.
|#

(println "example program: multiplication, written using locations")

(define mult8by3Locs (term (
  ((loc 0) := (num 8)) then
  (((loc 1) := (num 3)) then
  (((loc 10) := (num 0)) then
  (while ((loc 1)) do {
    (((loc 10) := ((loc 10) + (loc 0))) then
     ((loc 1) := ((loc 1) + (num -1))))}))))))

(judgment-holds (⇓ ()
	,mult8by3Locs
	: μ ) μ)

(println "example program: multiplication, written using variables")

(define mult8by3Vars (term
(let var increment := (num 8) in
(let var counter := (num 3) in
(let var result := (num 0) in
(while (counter) do {
  ((result := (result + increment)) then
   (counter := (counter + (num -1))))}))))))

(judgment-holds (⇓ ()
	,mult8by3Vars
	: μ ) μ)

#|
Simple example terms.
|#

(println "writing a value to a port")
(judgment-holds (⇓ (((port 8080) ((num 30))))
	(out((port 8080) < (num 10)))
	: μ ) μ)
(judgment-holds
 (⇓ () (out((port 8080) < (num 10))): μ )
 μ)
(judgment-holds (⇓ (((loc 1) (num 10)))
	(out((port 8080) < (loc 1)))
	: μ ) μ)

(println "conditional branching")
(judgment-holds (⇓ () 
	(if ((num 0)) 
		{ (out ((port 8080) < (num 10))) } else 
		{ (out ((port 8080) < (num 20))) } ) 
	: μ ) μ)
(judgment-holds (⇓ (((loc 1) (num 42))) 
	(if ((loc 1)) 
		{ (out ((port 8080) < (num 10))) } else 
		{ (out ((port 8080) < (num 20))) } ) 
	: μ ) μ)

(println "introducing a new variable into an empty scope")
(judgment-holds (⇓ () 
	(let var somevar := (num 10) in (out ((port 8080) < somevar)))
	: μ ) μ)
(println "introducing a new variable into an non-empty scope")
(judgment-holds (⇓ (((loc 1) (num 42))) 
	(let var somevar := (num 10) in (out ((port 8080) < somevar)))
	: μ ) μ)

(println "introducing a new variables: overshadowing")
(judgment-holds (⇓ () 
                   (let var somevar := (num 10) in
                     ((somevar := (num 20)) then
                      ((out ((port 8080) < somevar)) then
                      (let var somevar := (num 30) in
                      (out ((port 8080) < somevar))))))
                   : μ ) μ)

(println "assigning a expression to a location")
(judgment-holds (⇓ (((loc 1) (num 41))) 
	(((loc 1) := (num 42)) then (out((port 8080) < (loc 1))))
	: μ ) μ)

(println "while-loop and command sequence with 'then'")
(judgment-holds (⇓ () 
	((out((port 8080) < (num 10))) then (out((port 8080) < (num 20))))
	: μ ) μ)
(judgment-holds (⇓ ()
        (let var y := (num 30) in ((out((port 8080) < y)) then (out((port 8080) < (num 20)))))  
	: μ ) μ)

(println "adding two expressions together")
(judgment-holds (⇓ (((loc 1)(num -3))) 
	(let var somevar := ((num 32) + (loc 1)) in (out ((port 8080) < somevar)))
	: μ ) μ)
