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

(define cubo2Desor '(( (b an) (r b))
                     ( (b am) (a v))
                     ( (r a) (am v))
                     ( (am r) (an am))
                     ( (an v) (v a))
                     ( (a r) (b an)) ))

(define cubo3Desor '(( (a am an) (v b a) (am a am) )
                     ( (v a b) (r an v) (r b b) )
                     ( (r an am) (am am b) (r r v) )
                     ( (an b b) (a r r) (am r r) )
                     ( (a v a) (v v an) (an an b) )
                     ( (v am a) (b a an) (an am v) )))

(define cubo4Desor '(( (r a r a) (v b a v) (am r b b) (a b am a) )
                     ( (r r an an) (an b r v) (a am an a) (r v v an) )
                     ( (v an b am) (r am a an) (r a am b) (v a b b) )
                     ( (r b am v) (v r v am) (r b an a) (an r am am) )
                     ( (v an am b) (a a am b) (a r v am) (b an a am) )
                     ( (an v an b) (r an v r) (b an v am) (a v an am) )))

(RS
 3
 cubo3Desor
 '(F1D F3I C3B C2A F2D))

