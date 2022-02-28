#lang racket

;; Funcion principal
(define (RS X Cubo Movs)
 (cond ((or (< X 2) (< 6 X) )
        (quote (Cubo fuera de rango)))
       ((not (list? Movs))
        (quote (No es lista)))
       ((equal? (entradas_necesarias X) (entradas_ingresadas Cubo))
        (list (probar_comandos Cubo Movs))) ;;Aqui va toda la logica, de momento - Under construction -
       
       (else
        #f)
))

(define (probar_comandos Cubo Movs)
  (cond ((null? Movs)
         '())
        (else
         (cons (probar_comandos_aux Cubo (symbol->string (car Movs)))
               (probar_comandos Cubo (cdr Movs)))))
)

(define (probar_comandos_aux Cubo mov)
  (aplicarPosicion Cubo (string-ref mov 0) (string-ref mov 1) ) ;Falta incluir la dirección
)

(define (aplicarPosicion Cubo movimiento posicion) ;;Cambiar función por una recursiva
  (cond ((equal? movimiento '#\F) ;;Buscar la manera de quitar tanto if. Quiza convirtiendo #\1 en enteros.
         (cond ((equal? posicion '#\1)
                (Posicion_fila 1 Cubo))
               ((equal? posicion '#\2)
                (Posicion_fila 2 Cubo))
               ((equal? posicion '#\3)
                (Posicion_fila 3 Cubo))
               ((equal? posicion '#\4)
                (Posicion_fila 4 Cubo))
               ((equal? posicion '#\5)
                (Posicion_fila 5 Cubo))
               ((equal? posicion '#\6)
                (Posicion_fila 6 Cubo))))
        ((equal? movimiento '#\C)
         (cond ((equal? posicion '#\1)
                (Posicion_colum 1 (vuelta_al_cubo Cubo)))
               ((equal? posicion '#\2)
                (Posicion_colum 2 (vuelta_al_cubo Cubo)))
               ((equal? posicion '#\3)
                (Posicion_colum 3 (vuelta_al_cubo Cubo)))
               ((equal? posicion '#\4)
                (Posicion_colum 4 (vuelta_al_cubo Cubo)))
               ((equal? posicion '#\5)
                (Posicion_colum 5 (vuelta_al_cubo Cubo)))
               ((equal? posicion '#\6)
                (Posicion_colum 6 (vuelta_al_cubo Cubo)))))
        (else
         #f))
)

;; Funciones para girar una fila o columna. Toma el tercer argumento de una instruccion. Ej: F2D -> D
;; /Validos para todos los cubos, ya que siempre hay 4 lados independientemente de las dimensiones/
(define (Girar-izq fila)
  (list (cadr fila) (caddr fila) (cadddr fila) (car fila))
)

(define (Girar-der fila)
  (list (cadddr fila) (car fila) (cadr fila) (caddr fila) )
)

(define (Girar-ar colum)
  (list (cadr colum) (caddr colum) (cadddr colum) (car colum))
)

(define (Girar-ab colum)
  (list (cadddr colum) (car colum) (cadr colum) (caddr colum) )
)

;;Funciones para construir en una fila o columna n. Toma el segundo argumento de una instruccion. Ej: F2D -> 2
(define (Posicion_colum pos Cubo) ;; se recibe Cubo ya con vuelta_al_cubo aplicado
  (Columna (borrar-xc pos 4 Cubo) 4)
)

(define (Posicion_colum_aux pos Cubo) ;;Cambiar nombre de variable
  (cond ((> pos 1)
         #f);(borrar-1c))
        (else
         '()))
)

(define (Posicion_fila pos Cubo)
  (cond ((> pos 1)
         (Posicion_fila (- pos 1) (borrar-1f Cubo)))
        (else
         (Fila Cubo 4)))
)

;; Funciones para construir una fila o columna 360° (alrededor de las 4 caras del cubo). Toma el primer argumento de una instruccion. Ej: F2D -> F
(define (Fila Cubo cont)
  (cond ((> cont 0)
         (cons (caar Cubo) ;(Fila_aux Cubo)
                (Fila (cdr Cubo) (- cont 1))))
        (else
         '()))
)

(define (Columna Cubo cont) ; cont = 4 //Siempre son 4.
  (cond ((> cont 0)
         (cons (columna_aux (car Cubo) 3)
               (Columna (cdr Cubo) (- cont 1))))
        (else
         '()))
)

(define (columna_aux Cubo cont) ;; cont = 3 //Para el 3x3 solamente. Debe de cambiarse cont obteniendo X
  (cond ((> cont 0)
         (cons (caar Cubo) ;(Fila_aux Cubo)
               (columna_aux (cdr Cubo) (- cont 1))))
        (else
         '()))
)
;;_________________________________________________

;;________________________________________________________________________________________
;; Se ve despiche, pero es muy necesario. Solo revisar para cambiar nombres a funciones
;; Funciones para transponer el cubo //No servia con la funcion TRANSPUESTA. Hubo que crear esta
(define (vuelta_al_cubo cubo)
  (list (car cubo)
        (cadr (cddddr cubo))
        (voltear (caddr cubo3Desor))
        (car (cddddr cubo3Desor)))
)

(define (voltear lista)
  (voltear_aux (vuelta lista))
)

(define (voltear_aux lista)
  (cond ((null? lista)
         '())
        (else
         (cons (vuelta (car lista)) (voltear_aux (cdr lista)) ))))

(define (vuelta lista) ;; Se utiliza la pila para invertir una lista
  (vuelta_aux lista '())
)

(define (vuelta_aux lista listaNueva)
  (cond ((null? lista)
         listaNueva)
        (else
         (vuelta_aux (cdr lista) (cons (car lista) listaNueva))))
)
;;________________________________________________________________________________________

;; Funciones para obtener filas y columnas n > 1
(define (borrar-1f mat) ;; Borra la primera fila de una matriz
    (cond ((null? mat)
           '())
          (else
           (cons (cdr (car mat))
                 (borrar-1f (cdr mat))))))
                                       
(define (borrar-xc x cont mat) ;;Borra x columnas de una matriz // ; cont = 4 -> Numero de lados, pero puede ser 6? Ahora lo estudio
  (cond ((> cont 0)
         (cons (borrar-xc_aux x (car mat)) (borrar-xc x (- cont 1) (cdr mat))))
        (else
         '()))
)

(define (borrar-xc_aux x mat)
  (cond ((null? mat)
         '())
        (else
         (cons (borrar-xc_aux_aux x (car mat)) (borrar-xc_aux x (cdr mat)) )))
)

(define (borrar-xc_aux_aux x mat)
  (cond ((> x 1)
         (borrar-xc_aux_aux (- x 1) (cdr mat)))
        (else
         mat))
)
;;_______________________________________________

;;_______________________________________________
;; Para la validación ;;
(define (entradas_necesarias num)
  (* num num 6) ;6 es el numero de caras en todas las dimensiones del cubo (2x2, 3x3, 4x4..)
)

(define (entradas_ingresadas lista)
  (cond ((not (equal? (largo lista) 6))
         #f)
        (else
         (largo (convertirALista (convertirAMatriz lista)))))
  )
;;_______________________________________________

(define (convertirALista matriz) ; Matriz -> Lista ;; ** Cambiar nombre por obtenerLista **
  (cond ((>= (largo matriz) 1)
         (convertir_aux matriz matriz)
               )
        (else
         '())
        ))

(define (convertirAMatriz matriz) ; Entradas de cubo -> Matriz | Convierte las entradas del cubo en matriz
  (cond ((>= (largo matriz) 1)
         (cons (convertir_aux (car matriz) (car matriz))
               (convertirAMatriz (cdr matriz))))
        (else
         '())
        )
)

(define (convertir_aux matriz matrizPrevia)
  (cond ((null? matrizPrevia)
         '())
        ((null? matriz)
         (convertir_aux (cdr matrizPrevia) (cdr matrizPrevia)))
        ((symbol? (car matriz))
         (cons (car matriz) (convertir_aux (cdr matriz) matrizPrevia)))
        (else
         (convertir_aux (car matriz) matrizPrevia)))
)

;;___________________________________
;; Largo
(define (largo lista)
  (cond ((null? lista)
         0)
        (else
         (+ 1 (largo (cdr lista))))))
;;___________________________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Para pruebas
(define cubo3 '(( (v v v) (v v v) (v v v) )
                ( (r r r) (r r r) (r r r) )
                ( (a a a) (a a a) (a a a) )
                ( (an an an) (an an an) (an an an) )
                ( (b b b) (b b b) (b b b) )
                ( (am am am) (am am am) (am am am) )))

(define cubo3F1D '(( (an an an) (v v v) (v v v) )
                   ( (v v v) (r r r) (r r r) )
                   ( (r r r) (a a a) (a a a) )
                   ( (a a a) (an an an) (an an an) )
                   ( (b b b) (b b b) (b b b ) )
                   ( (am am am) (am am am) (am am am)) ))

(define cubo3raro '(((b am b) (am b am) (b am b))
                    ((v a v) (a v a) (v a v))
                    ((am b am) (b am b) (am b am))
                    ((a v a) (v a v) (a v a))
                    ((r an r) (an r an) (r an r))
                    ((an r an) (r an r) (an r an))))

(define cubo3Desor '(( (r an am) (b b an) (am am r) )
                     ( (an b r) (b r am) (b v b) )
                     ( (b am b) (v am r) (an a r) )
                     ( (an r a) (v an r) (am am v) )
                     ( (v r v) (a a a) (am v a) )
                     ( (an an a) (a v b) (v an a) )))