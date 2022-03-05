#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

(define-struct cube(estado))

(define initial (make-cube '(((R R R R)(R R R R)(R R R R) (R R R R))
                     ((G G G G)(G G G G)(G G G G) (G G G G))
                     ((O O O O)(O O O O)(O O O O) (O O O O)))))

(define ESTADOS '())
(define DIMENSION 0)
(define contEstados 0)

(define (getColor letra)
  (cond ((equal? letra 'R) "Firebrick")
        ((equal? letra 'W) "Snow" )
        ((equal? letra 'G) "Medium Sea Green")
        ((equal? letra 'B) "Royal Blue")
        ((equal? letra 'O) "Dark Orange")
        ((equal? letra 'Y) "Goldenrod")
        (else "black" )))

(define (transformarAForma lista cara)
  (cond ((null? lista) '())
        ((list? (car lista)) (append (transformarAForma (car lista) cara) (transformarAForma (cdr lista) cara)))
        (else
         (cons (getFigura cara (car lista)) (transformarAForma (cdr lista) cara)))))

(define (getFigura cara color)
  (cond ((equal? cara "centro")
         (underlay (polygon (list (make-posn 0 (+ TAMAÑO MITAD))
                        (make-posn 0 MITAD)
                        (make-posn TAMAÑO 0)
                        (make-posn TAMAÑO TAMAÑO)) "solid" (getColor color))
                   (polygon (list (make-posn 0 (+ TAMAÑO MITAD))
                        (make-posn 0 MITAD)
                        (make-posn TAMAÑO 0)
                        (make-posn TAMAÑO TAMAÑO)) "outline" "black")))
        ((equal? cara "izquierda")
         (underlay (polygon (list (make-posn 0 TAMAÑO)
                        (make-posn 0 0)
                        (make-posn TAMAÑO MITAD)
                        (make-posn TAMAÑO (+ MITAD TAMAÑO))) "solid" (getColor color))
                   (polygon (list (make-posn 0 TAMAÑO)
                        (make-posn 0 0)
                        (make-posn TAMAÑO MITAD)
                        (make-posn TAMAÑO (+ MITAD TAMAÑO))) "outline" "black")))
        ((equal? cara "arriba")
         (underlay (polygon (list (make-posn TAMAÑO 0)
                        (make-posn 0 MITAD)
                        (make-posn TAMAÑO (+ MITAD MITAD))
                        (make-posn (+ TAMAÑO TAMAÑO) MITAD)) "solid" (getColor color))
                   (polygon (list (make-posn TAMAÑO 0)
                        (make-posn 0 MITAD)
                        (make-posn TAMAÑO (+ MITAD MITAD))
                        (make-posn (+ TAMAÑO TAMAÑO) MITAD)) "outline" "black")))))

(define (estadoCubo lista)
  (append (transformarAForma (list-ref lista 0) "arriba")
          (transformarAForma (list-ref lista 1) "izquierda")
          (transformarAForma (list-ref lista 2) "centro")))

(define (posicionCubo2 lista)
  (append (listaAPuntos (list-ref lista 0) "arriba" (+ (* TAMAÑO 1.5) 100) (+ (* TAMAÑO 2.5) 100))
  (listaAPuntos (list-ref lista 1) "izquierda" (+ TAMAÑO 100) (+(* TAMAÑO 3) (/ MITAD 2) 100))
  (listaAPuntos (list-ref lista 2) "centro" (+ (* TAMAÑO 5) 100) (+(-(* TAMAÑO 5) (* MITAD 1.5)) 100))))


(define (posicionCubo lista)
  (define CENTROx '())
  (define CENTROy '())
  (cond ((= DIMENSION 3)
         (set! CENTROx '(67 42 192))
         (set! CENTROy '(130 165 205)))
        ((= DIMENSION 4)
         (set! CENTROx '(56.25 36 187))
         (set! CENTROy '(128.75 155 200)))
        ((= DIMENSION 5)
         (set! CENTROx '(51.75 36 187))
         (set! CENTROy '(130.5 152 200)))
        ((= DIMENSION 6)
         (set! CENTROx '(48.75 36 187))
         (set! CENTROy '(131.75 150 200))))
  
  (append (listaAPuntos (list-ref lista 0) "arriba" (list-ref CENTROx 0) (list-ref CENTROy 0))
  (listaAPuntos (list-ref lista 1) "izquierda" (list-ref CENTROx 1) (list-ref CENTROy 1))
  (listaAPuntos (list-ref lista 2) "centro" (list-ref CENTROx 2) (list-ref CENTROy 2))))

(define (listaAPuntos lista cara x y) (transformarAPuntos lista cara 0 0 x y))

(define (transformarAPuntos lista cara fila col x y)
  (cond ((null? lista) '())
        ((list? (car lista)) (append (transformarAPuntos (car lista) cara fila col x y) (transformarAPuntos (cdr lista) cara (+ fila 1) col x y)))
        ((equal? cara "arriba")
         (cons (make-posn (+ (+ x (* TAMAÑO fila)) (* TAMAÑO col)) (+ (- y (* MITAD fila)) (* MITAD col))) (transformarAPuntos (cdr lista) cara fila (+ col 1) x y)))
        ((equal? cara "izquierda")
         (cons (make-posn (+ x (* TAMAÑO fila)) (+ (+ y (* TAMAÑO col)) (* MITAD fila))) (transformarAPuntos (cdr lista) cara fila (+ col 1) x y)))
        ((equal? cara "centro")
         (cons (make-posn (+ x (* TAMAÑO fila)) (- (+ y (* TAMAÑO col)) (* MITAD fila))) (transformarAPuntos (cdr lista) cara fila (+ col 1) x y)))))


(define (draw-cube c)
  (place-images
   (estadoCubo (cube-estado c))
   (posicionCubo (cube-estado c))
   (empty-scene 500 500)))

(define TAMAÑO "unknown")
(define MITAD "unknown")

(define (changeStatus w key)
  (cond ((key=? key "left")
         (cond ((> contEstados 0) (set! contEstados (- contEstados 1))))
         (set! DIMENSION (length (car (list-ref ESTADOS contEstados))))
         (display contEstados)
         (set! TAMAÑO ( / 150 DIMENSION))
         (set! MITAD (/ TAMAÑO 2.5))
         (make-cube (list-ref ESTADOS contEstados)))
        
        ((key=? key "right")
         (display (< contEstados (length ESTADOS)))
         (cond ((< contEstados (-(length ESTADOS)1)) (set! contEstados (+ contEstados 1))))
         (set! DIMENSION (length (car (list-ref ESTADOS contEstados))))
         (display contEstados)
         (set! TAMAÑO ( / 150 DIMENSION))
         (set! MITAD (/ TAMAÑO 2.5))
         (make-cube (list-ref ESTADOS contEstados)))
        (else w)))

(define (inicio listaEstados)
  (set! ESTADOS listaEstados)
  (set! DIMENSION 4)
  (set! TAMAÑO ( / 150 DIMENSION))
  (set! MITAD (/ TAMAÑO 2.5))
  (big-bang initial [to-draw draw-cube]
    [on-key changeStatus]
    ))

(inicio '((((O R G)(G W R)(O O Y))
           ((R W W)(O O O)(R Y R))
           ((G B R)(R B R)(Y G G)))

          (((O R G)(O O O)(O R R))
           ((R W W)(R B R)(R Y R))
           ((G B R)(G W R)(Y G G)))

          (((O R G)(O O O)(O O Y))
           ((R W W)(R B R)(R Y R))
           ((G B R)(G W R)(Y G G)))

          (((O R G)(O O O)(O O Y))
           ((R W W)(R B R)(R Y R))
           ((G B R)(G W R)(Y G G)))))



