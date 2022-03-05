#lang racket
(require "rubik.rkt")
(require "interfaz.rkt")


(define (RS X Cubo Movs)
  (inicio  (RS_aux X Cubo Movs)))

(define (RS_aux X Cubo Movs)
  (cond
    ((null? Movs)empty)
    (else
     (cons (carasFLU (estadosDelCubo X Cubo (list (car Movs))))
           (RS_aux
            X
            (estadosDelCubo X Cubo (list (car Movs)))
            (cdr Movs)) ))))

(RS
 3
 '(( (v v v) (v v v) (v v v) )
   ( (r r r) (r r r) (r r r) )
   ( (a a a) (a a a) (a a a) )
   ( (an an an) (an an an) (an an an) )
   ( (b b b) (b b b) (b b b) )
   ( (am am am) (am am am) (am am am) ))
 '(F1D F1I F2D F3I))

