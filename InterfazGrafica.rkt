#lang racket
(provide iniciarInterfaz)
;Interfaz gráfica Rubiks by
;Ignacio Grané
;Andy Ramírez 
;Juan Rodríguez

;librerías necesitadas
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;Estructura de cubo que ayudará a ilustrar en el frame
(define-struct cubo(estado))

;estado inicial del cubo, no ahy ningún dato, es un cubo en negro
(define initial (make-cubo '(((1 1 1)(1 1 1)(1 1 1))
                             ((1 1 1)(1 1 1)(1 1 1))
                             ((1 1 1)(1 1 1)(1 1 1)))))
;Defininción de variables globales

;lista de movimientos hechos por el usuariol
(define ESTADOS '())
;Dimensión del Cubo (2x2 3x3 4x4 5x5 6x6)
(define DIMENSION 0)
;Tamaño gráficos de los cuadrados
(define TAMAÑO "unknown")
;Tamaño medio para posiciones gráficas
(define MITAD "unknown")
;Estado representado en la interfaz 
(define ESTADOACTUAL "unknown")

;Retorna el nombre del color según la variable definida para los colores, en caso de no ser ningún color, devuelve negro
(define (obtenerColor letra)
  (cond ((equal? letra 'r) "Firebrick")
        ((equal? letra 'b) "Snow" )
        ((equal? letra 'v) "Medium Sea Green")
        ((equal? letra 'a) "Royal Blue")
        ((equal? letra 'an) "Dark Orange")
        ((equal? letra 'am) "Goldenrod")
        (else "black" )))

;Según la cara (arriba, centro o izquierda) retorna la figura a ser dibujada
(define (obtenerFigura cara color)
  ;Romboide 
  (cond ((equal? cara "centro")
         (underlay (polygon (list (make-posn 0 (+ TAMAÑO MITAD))
                                  (make-posn 0 MITAD)
                                  (make-posn TAMAÑO 0)
                                  (make-posn TAMAÑO TAMAÑO)) "solid" (obtenerColor color))
                   (polygon (list (make-posn 0 (+ TAMAÑO MITAD))
                                  (make-posn 0 MITAD)
                                  (make-posn TAMAÑO 0)
                                  (make-posn TAMAÑO TAMAÑO)) "outline" "black")))
        ;Romboide en espejo
        ((equal? cara "izquierda")
         (underlay (polygon (list (make-posn 0 TAMAÑO)
                                  (make-posn 0 0)
                                  (make-posn TAMAÑO MITAD)
                                  (make-posn TAMAÑO (+ MITAD TAMAÑO))) "solid" (obtenerColor color))
                   (polygon (list (make-posn 0 TAMAÑO)
                                  (make-posn 0 0)
                                  (make-posn TAMAÑO MITAD)
                                  (make-posn TAMAÑO (+ MITAD TAMAÑO))) "outline" "black")))
        ;Rombo
        ((equal? cara "arriba")
         (underlay (polygon (list (make-posn TAMAÑO 0)
                                  (make-posn 0 MITAD)
                                  (make-posn TAMAÑO (+ MITAD MITAD))
                                  (make-posn (+ TAMAÑO TAMAÑO) MITAD)) "solid" (obtenerColor color))
                   (polygon (list (make-posn TAMAÑO 0)
                                  (make-posn 0 MITAD)
                                  (make-posn TAMAÑO (+ MITAD MITAD))
                                  (make-posn (+ TAMAÑO TAMAÑO) MITAD)) "outline" "black")))))

;Recibe la matriz de una cara y cuál cara es, la trasforma en una lista de figuras a para ser dibujadas
(define (transformarAForma matriz cara)
  (cond ((null? matriz) '())
        ((list? (car matriz)) (append (transformarAForma (car matriz) cara) (transformarAForma (cdr matriz) cara)))
        (else
         (cons (obtenerFigura cara (car matriz)) (transformarAForma (cdr matriz) cara)))))

;Recibe la matriz de estado, y transforma cada cara en una lista de figuras, las une en una lista mayor
(define (obtenerCuboGrafico lista)
  ;Unión de listas
  (append (transformarAForma (list-ref lista 2) "arriba")
          (transformarAForma (list-ref lista 1) "izquierda")
          (transformarAForma (list-ref lista 0) "centro")))

;Recibe la matriz de una cara, y la trasforma en lista de puntos
(define (transformarAPuntos matriz cara col fila x y)
  (cond ((null? matriz) '())
        ;Si es una lista, envia une la lista de puntos de la primera fila, y hace recursividad con las filas restantes
        ((list? (car matriz)) (append (transformarAPuntos (car matriz) cara col fila x y)
                                      (transformarAPuntos (cdr matriz) cara col (+ fila 1) x y)))
        
        ;utilizando el siguiente algoritmo: se suman a (x y) un TAMAÑO o una MITAD 
        ;por cada fila o por cada columna según cada cara:
        ((equal? cara "arriba")
         (cons (make-posn (+ (+ x (* TAMAÑO col)) (* TAMAÑO fila))
                          (+ (- y (* MITAD col)) (* MITAD fila)))
               (transformarAPuntos (cdr matriz) cara (+ col 1) fila x y)))
        ((equal? cara "izquierda")
         (cons (make-posn (+ x (* TAMAÑO col))
                          (+ (+ y (* TAMAÑO fila)) (* MITAD col)))
               (transformarAPuntos (cdr matriz) cara (+ col 1) fila x y)))
        ((equal? cara "centro")
         (cons (make-posn (+ x (* TAMAÑO col))
                          (- (+ y (* TAMAÑO fila)) (* MITAD col)))
               (transformarAPuntos (cdr matriz) cara (+ col 1) fila x y)))))

;Recibe la lista de matrices del estado de cubo y la trasforma una lista de puntos específicos (segúun librería posn) donde imprimirlos en un espacio
(define (obtenerCuboPosicion lista)
  ;Puntos de inicio para cada cara según su dimensión
  (define CENTROx '())
  (define CENTROy '())
  (cond ((= DIMENSION 2)
         (set! CENTROx '(113.5 76 226))
         (set! CENTROy '(130 182 212)))
        ((= DIMENSION 3)
         (set! CENTROx '(88 63 213))
         (set! CENTROy '(130 165 205)))
        ((= DIMENSION 4)
         (set! CENTROx '(76.25 57 207))
         (set! CENTROy '(128.75 155 200)))
        ((= DIMENSION 5)
         (set! CENTROx '(68.75 53.25 203.5))
         (set! CENTROy '(131 152 200)))
        ((= DIMENSION 6)
         (set! CENTROx '(63.75 51.5 201.5))
         (set! CENTROy '(132.25 150 200))))
  ;Unión de listas
  (append (transformarAPuntos (list-ref lista 0) "arriba" 0 0 (list-ref CENTROx 0) (list-ref CENTROy 0))
          (transformarAPuntos (list-ref lista 1) "izquierda" 0 0 (list-ref CENTROx 1) (list-ref CENTROy 1))
          (transformarAPuntos (list-ref lista 2) "centro" 0 0 (list-ref CENTROx 2) (list-ref CENTROy 2))))

;Función que dibuja la interfaz y los cambios de estado
(define (draw-cube c)
  (place-images
   (obtenerCuboGrafico (cubo-estado c))
   (obtenerCuboPosicion (cubo-estado c))
   (scale (/ 1 2.9)  (bitmap/file "fondo.png"))))

;Control de errores gráficos, para cada dimesión los tamaños de cada cuadrado impreso
;Con esta función, todas las dimesiones de cubos mantendrán un tamaño prestablecido de 150 pixeles
(define (redimencionar tam)
  (set! DIMENSION tam)
  (set! TAMAÑO ( / 150 DIMENSION))
  (set! MITAD (/ TAMAÑO 2.5)))

;Función pra el evento de una tecla, si son son flechas, se envía a dibujar la matriz representando el estado actual, según los casos:
;Se tiene el control de errores redimensionar
(define (cambioEstado w key)
  (cond (
         ;Izquierda: se resta 1 al estado actual, en caso de ser 0, se mantiene en 0.
         (key=? key "left")
         (cond ((equal? ESTADOACTUAL "unknown")
                (set! ESTADOACTUAL 0))
               ((> ESTADOACTUAL 0)
                (set! ESTADOACTUAL (- ESTADOACTUAL 1))))
         ;Control de errores de dimención antes de imprimir
         (redimencionar (length (car (list-ref ESTADOS ESTADOACTUAL))))
         (make-cubo (list-ref ESTADOS ESTADOACTUAL)))
        ;Derecha: se suma 1 al estado actual, en caso de ser mayor al largo de los estados, se mantiene en el último estado disponible.
        ((key=? key "right")
         (cond ((equal? ESTADOACTUAL "unknown")
                (set! ESTADOACTUAL 0))
               ((< ESTADOACTUAL (-(length ESTADOS)1))
                (set! ESTADOACTUAL (+ ESTADOACTUAL 1))))
         ;Control de errores de dimención antes de imprimir
         (redimencionar (length (car (list-ref ESTADOS ESTADOACTUAL))))
         (make-cubo (list-ref ESTADOS ESTADOACTUAL)))
        (else w)))

;Función principal, recibe una lista de matrices de todos los estados a ser dibujados y redefine la variable global
;El cubo inicial será en negro,, se utiliza big-bang como inicio de interfaz gráfica según la librería 2htdp/universe
(define (iniciarInterfaz listaEstados)
  (set! ESTADOS listaEstados)
  (redimencionar 3)
  (big-bang initial
    ;Función de dibujar
    [to-draw draw-cube]
    ;Función de evento Tecla
    [on-key cambioEstado]
    ))




