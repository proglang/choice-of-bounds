#lang racket

(require redex "../src/TransformToXLabels.rkt" "../src/Optimize.rkt" "../src/Grammar.rkt")


; The program that will be used in every example.

(define exampleProgram (term
                        ((aVar := password)
                         then
                         (if (aVar)
                             {(out ((port 1) < (num 42)))} else
                             {(out ((port 1) < (num -1)))}))))

; The static type of the output port.

(define portEnv (term (((port 1) ((3 4 5))))))


#|
The label of the variable pc can have either the type (2) or (3).
If it is (2), the program may not write onto the output port.
If it is (3), writing to the output port is legal. Since pc can have one of either types at runtime, we have to dynamically check the label of pc at runtime to ensure
no unsecure writing happens.
|#

(println "1. Dynamic check is necessary.")

(define indFlowTypes (term ((labelvar-pc ((2)(3)))(pc ((2)(3))) (labelvar-aVar ((3))) (aVar ((3))) (labelvar-password ((5)))(password ((5))))))

#|
The label of pc can have only the type (3).
No check is necessary, and output can be written to the ports, since the label of pc, (3) is lower or equal to the label of port 1, (3 4 5).
|#

(term (optimize (transformToExplicitLabels ,portEnv ,indFlowTypes () ,exampleProgram) ,indFlowTypes))

(println "2. Dynamic check is not necessary because output will always succeed.")

(define indFlowSucEnv (term ((labelvar-pc ((3)))(pc ((3))) (labelvar-aVar ((3))) (aVar ((3))) (labelvar-password ((5)))(password ((5))))))

(term (optimize (transformToExplicitLabels ,portEnv ,indFlowSucEnv () ,exampleProgram) ,indFlowSucEnv))

#|
The label of pc can have either the type (2) or (6).
No check is necessary, since either of these labels violate the port's label (3 4 5).
|#

(println "3. Dynamic check is not necessary because output will always fail.")

(define indFlowFailEnv (term ((labelvar-pc ((2) (6)))(pc ((2) (6))) (labelvar-aVar ((3))) (aVar ((3))) (labelvar-password ((5)))(password ((5))))))

(term (optimize (transformToExplicitLabels ,portEnv ,indFlowFailEnv () ,exampleProgram) ,indFlowFailEnv))




