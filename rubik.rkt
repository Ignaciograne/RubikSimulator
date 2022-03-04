#lang racket

(define (obtenerMov movimiento)
  (obtenerMov_aux (string-ref (symbol->string movimiento) 0))
)

(define (obtenerMov_aux mov)
  (cond ((equal? mov '#\F)
         'F)
        ((equal? mov '#\C)
         'C)
        (else
         #f))
)

(define (obtenerPos movimiento)
  (obtenerPos_aux (string-ref (symbol->string movimiento) 1))
)

(define (obtenerPos_aux mov)
  (cond ((equal? mov '#\1)
         '1)
        ((equal? mov '#\2)
         '2)
        ((equal? mov '#\3)
         '3)
        ((equal? mov '#\4)
         '4)
        ((equal? mov '#\5)
         '5)
        ((equal? mov '#\6)
         '6)
        (else
         #f))
)

(define (obtenerDir movimiento)
  (obtenerDir_aux (string-ref (symbol->string movimiento) 2))
)

(define (obtenerDir_aux mov)
  (cond ((equal? mov '#\D)
         'D)
        ((equal? mov '#\I)
         'I)
        ((equal? mov '#\A)
         'A)
        ((equal? mov '#\B)
         'B)
        (else
         #f))
)

;; Funcion principal
(define (RS X Cubo Movs)
 (cond ((or (< X 2) (< 6 X) ) ; Valida las dimensiones
        (quote (Cubo fuera de rango)))
       ((not (list? Movs)) ; Valida los movimientos *;; Todavia se puede validar por cada letra *
        (quote (No es lista)))
       ((equal? (entradas_necesarias X) (entradas_ingresadas Cubo)) ; Valida las entradas *;; Todavia se puede validar por cada letra *
        (ejecutar_movimientos Cubo Movs)) ;;Aqui va toda la logica, de momento - Under construction -
       
       (else
        #f)
))

(define (ejecutar_movimientos Cubo Movs)
  (cond ((null? Movs)
         Cubo)
        (else
         (ejecutar_movimientos_aux Cubo (car Movs) (cdr Movs) )))
)

(define (obtener_foc Cubo Mov) ; FoC = Fila o Columna
  (cond ((equal? (obtenerMov Mov) 'F)
         (FilaColumna Cubo (obtenerPos Mov)))
        ((equal? (obtenerMov Mov) 'C)
         (FilaColumna Cubo (obtenerPos Mov)))) ;(FilaColumna (reordenar_cubo (voltear_cubo Cubo)) (obtenerPos Mov))
)

(define (ejecutar_movimientos_aux Cubo Mov Movs)
  (cond ((equal? (obtenerDir Mov) 'A) ; Si es A o B, debe de ser un movimiento de columna
         (ejecutar_movimientos (re_reordenar_cubo (devolver_cubo (construir_cubo (reordenar_cubo (voltear_cubo Cubo)) Mov (GirarDer (obtener_foc (reordenar_cubo (voltear_cubo Cubo)) Mov)) (darVuelta_aux (transpuesta (car (cddddr (reordenar_cubo (voltear_cubo Cubo))))) '()) (darVuelta (transpuesta (car (cdr (cddddr (reordenar_cubo (voltear_cubo Cubo))))))) ))) Movs))
        ((equal? (obtenerDir Mov) 'B)
         (ejecutar_movimientos (re_reordenar_cubo (devolver_cubo (construir_cubo (reordenar_cubo (voltear_cubo Cubo)) Mov (GirarIzq (obtener_foc (reordenar_cubo (voltear_cubo Cubo)) Mov)) (darVuelta (transpuesta (car (cddddr (reordenar_cubo (voltear_cubo Cubo)))))) (darVuelta_aux (transpuesta (car (cdr (cddddr (reordenar_cubo (voltear_cubo Cubo)))))) '()) ))) Movs))
        ((equal? (obtenerDir Mov) 'I) ; Si es I o D, debe de ser un movimiento de fila
         (ejecutar_movimientos (construir_cubo Cubo Mov (GirarIzq (obtener_foc Cubo Mov)) (darVuelta (transpuesta (car (cddddr Cubo)))) (darVuelta_aux (transpuesta (car (cdr (cddddr Cubo)))) '()) ) Movs))
        ((equal? (obtenerDir Mov) 'D)
         (ejecutar_movimientos (construir_cubo Cubo Mov (GirarDer (obtener_foc Cubo Mov)) (darVuelta_aux (transpuesta (car (cddddr Cubo))) '()) (darVuelta (transpuesta (car (cdr (cddddr Cubo))))) ) Movs))
        )
)

(define (construir_cubo Cubo Mov filaColumnaGirada quintaCaraGirada sextaCaraGirada) ;; Construye los 6 lados del cubo, tomando la fila/columna girada y las otras dos caras
  (cond ((lateralSuperior? Mov)
         (convertirACubo (completar_cubo Cubo filaColumnaGirada Mov) quintaCaraGirada (car (cdr (cddddr Cubo))))) ;; Aqui se construye el cubo
        ((lateralInferior? Cubo Mov)
         (convertirACubo (completar_cubo Cubo filaColumnaGirada Mov) (car (cddddr Cubo)) sextaCaraGirada))
        (else
         (convertirACubo (completar_cubo Cubo filaColumnaGirada Mov) (car (cddddr Cubo)) (car (cdr (cddddr Cubo)))) ))
)

(define (convertirACubo primerasCuatroCaras quintaCara sextaCara) ;; Esta funcion no me representa :v Pero a falta de append
  (cond ((not (null? primerasCuatroCaras))
         (cons (car primerasCuatroCaras)
               (convertirACubo (cdr primerasCuatroCaras) quintaCara sextaCara) ))
        ((not (null? quintaCara))
         (cons quintaCara
               (convertirACubo primerasCuatroCaras '() sextaCara) ))
        ((not (null? sextaCara))
         (cons sextaCara
               (convertirACubo primerasCuatroCaras '() '()) ))
        (else
         '()))
)

;(define ())

(define (reordenar_cubo Cubo) ; Para el cambio de columnas a filas
  (reordenar_cubo_aux (car Cubo) (car (cddddr Cubo)) (darVuelta (darVuelta_aux (car (cddr Cubo)) '())) (car (cdr (cddddr Cubo))) (car (cdddr Cubo)) (car (cdr Cubo)))
)

(define (re_reordenar_cubo Cubo) ; Para el cambio de filas a columnas
  (reordenar_cubo_aux (car Cubo) (car (cdr (cddddr Cubo))) (darVuelta (darVuelta_aux (car (cddr Cubo)) '())) (car (cddddr Cubo)) (car (cdr Cubo))  (car (cdddr Cubo)))
)

(define (reordenar_cubo_aux primerCara quintaCara tercerCara sextaCara cuartaCara segundaCara) ;; Esta funcion no me representa x2 :v Pero a falta de append
  (cond ((not (null? primerCara))
         (cons primerCara
               (reordenar_cubo_aux '() quintaCara tercerCara sextaCara cuartaCara segundaCara) ))
        ((not (null? quintaCara))
         (cons quintaCara
               (reordenar_cubo_aux '() '() tercerCara sextaCara cuartaCara segundaCara) ))
        ((not (null? tercerCara))
         (cons tercerCara
               (reordenar_cubo_aux '() '() '() sextaCara cuartaCara segundaCara) ))
        ((not (null? sextaCara))
         (cons sextaCara
               (reordenar_cubo_aux '() '() '() '() cuartaCara segundaCara) ))
        ((not (null? cuartaCara))
         (cons cuartaCara
               (reordenar_cubo_aux '() '() '() '() '() segundaCara) ))
        ((not (null? segundaCara))
         (cons segundaCara
               (reordenar_cubo_aux '() '() '() '() '() '()) ))
        (else
         '()))
)

(define (completar_cubo Cubo filaColumna Mov)
  (cond ((null? filaColumna)
         '())
        (else
         (cons (completar_cubo_aux (car Cubo) (car filaColumna) (obtenerPos Mov) 1)
               (completar_cubo (cdr Cubo) (cdr filaColumna) Mov) )))
)

(define (completar_cubo_aux cara filaColumna pos cont)
  (cond ((null? cara)
         '())
        ((equal? cont pos)
         (cons filaColumna
               (completar_cubo_aux (cdr cara) filaColumna pos (+ cont 1) ) ))
        (else
         (cons (car cara)
               (completar_cubo_aux (cdr cara) filaColumna pos (+ cont 1) ))))
)

(define (lateralSuperior? Mov)
  (cond ((equal? (obtenerPos Mov) 1)
         #t)
        (else
         #f))
)

(define (lateralInferior? Cubo Mov)
  (cond ((equal? (obtenerPos Mov) (dimensiones Cubo) )
         #t)
        (else
         #f))
)

(define (darVuelta_aux cara caraVuelta) ;; La vuelta hacia la derecha ; La cara que recibe tiene que estar TRANSPUESTA
  (cond ((null? cara)
         caraVuelta)
        (else
         (darVuelta_aux (cdr cara) (cons (car cara) caraVuelta) )))
)

(define (darVuelta cara) ;; La vuelta hacia la izquierda ; La cara que recibe tiene que estar TRANSPUESTA
  (cond ((null? cara)
         '())
        (else
         (cons (darVuelta_aux (car cara) '() )
               (darVuelta (cdr cara)) )))
)

(define (voltear_cubo Cubo)
  (cond ((null? Cubo)
         '())
        (else
         (cons (darVuelta (transpuesta (car Cubo)))
               (voltear_cubo (cdr Cubo)) )))
)

(define (devolver_cubo Cubo) ;; Voltea el cubo hasta que este vuelva a su estado inicial
  (voltear_cubo (voltear_cubo (voltear_cubo Cubo)))
  ;(transponer (voltear_cubo (transponer (voltear_cubo cubo3Desor))))
)

(define (FilaColumna Cubo pos)
  (cond ((not (null? Cubo))
         (cons (FilaColumna_aux (car Cubo) pos 1)
               (FilaColumna (cdr Cubo) pos)))
        (else
         '()))
)

(define (FilaColumna_aux Cubo pos cont)
  (cond ((> pos cont)
         (FilaColumna_aux (cdr Cubo) pos (+ cont 1)))
        (else
         (car Cubo)))
)

;; Para poder girar
(define (GirarIzq filaColumna) ; Para girar hacia la izquierda
  (GirarIzq_aux (cdr filaColumna) (car filaColumna) 1)
)

(define (GirarIzq_aux filaColumna primerFilaColumna cont)
  (cond ((zero? cont)
         '())
        ((<= cont 3)
         (cons (car filaColumna)
               (GirarIzq_aux (cdr filaColumna) primerFilaColumna (+ cont 1)) ))
        (else
         (cons primerFilaColumna
               (GirarIzq_aux filaColumna primerFilaColumna 0)))) ; cont = 0 como condicion de parada
)

(define (GirarDer filaColumna)
  (GirarDer_aux (cadddr filaColumna) filaColumna 1)
)

(define (GirarDer_aux primerFilaColumna filaColumna cont)
  (cond ((equal? cont 5)
         '())
        ((equal? cont 1)
         (cons primerFilaColumna
               (GirarDer_aux primerFilaColumna filaColumna (+ cont 1)) ))
        (else
         (cons (car filaColumna)
               (GirarDer_aux primerFilaColumna (cdr filaColumna) (+ cont 1) ))))
)

;; Para obtener columnas se necesita la transpuesta
(define (transponer mat)
  (cond ((not (null? mat))
         (cons (transpuesta (car mat))
               (transponer (cdr mat)) ))
        (else
         '()))
)

(define (transpuesta mat)
  (cond ((null? (car mat))
         '())
        (else
         (cons (sacar-1f mat)
               (transpuesta (borrar-1f mat))))))

(define (sacar-1f mat)
  (cond ((null? mat)
         '())
        (else
         (cons (car (car mat))
               (sacar-1f (cdr mat)))))
)

(define (borrar-1f mat)
  (cond ((null? mat)
         '())
        (else
         (cons (cdr (car mat))
               (borrar-1f (cdr mat)))))
)

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
         (convertir_aux matriz matriz))
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

;; Dimensiones de cubo
(define (dimensiones lista)
  (largo (car lista)))
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