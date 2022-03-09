#lang racket
(require "rubik.rkt")
(require "InterfazGrafica.rkt")

(define (RS X Cubo Movs)
  (iniciarInterfaz  (RS_aux X Cubo Movs)))

(define (RS_aux X Cubo Movs)
  (cond
    ((null? Movs)empty)
    (else
     (cons (carasFLU (estadosDelCubo X Cubo (list (car Movs))))
           (RS_aux
            X
            (estadosDelCubo X Cubo (list (car Movs)))
            (cdr Movs)) ))))



(define cubo4Desor '(( (r a r a) (v b a v) (am r b b) (a b am a) )
                     ( (r r an an) (an b r v) (a am an a) (r v v an) )
                     ( (v an b am) (r am a an) (r a am b) (v a b b) )
                     ( (r b am v) (v r v am) (r b an a) (an r am am) )
                     ( (v an am b) (a a am b) (a r v am) (b an a am) )
                     ( (an v an b) (r an v r) (b an v am) (a v an am) )))


(RS
 4
cubo4Desor
 '(F1D F1I C2B))


