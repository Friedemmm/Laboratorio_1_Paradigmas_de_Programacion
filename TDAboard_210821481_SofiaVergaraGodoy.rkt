#lang racket

(provide (all-defined-out))

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")
(require "TDApiece_2108221481_SofiaVergaraGodoy.rkt")

;----------------CONSTRUCTOR-BOARD----------------;

; Descripción: Crea un tablero de 7 columnas y 6 filas lleno de ceros, donde se hacen 6 listas con 7 elementos.
; Dom: No recibe parámetros de entrada.
; Rec: Retorna una lista de listas con pares que representa el tablero vacío.
; Tipo recursión: No aplica.

; // (define empty-board (board))

; Función para crear el tablero Conecta 4.
(define (board)
  ; Función auxiliar para crear fila.
  (define make-fila
    (crear-lista-especifica 7 0) ; (make-list "Número de elementos" "Elementos dentro").
    )
  
  ; Crear 6 listas con 7 elementos que serian las columnas.
  (crear-lista make-fila
               make-fila
               make-fila
               make-fila
               make-fila
               make-fila
               )
  )

;----------------MODIFICADORES----------------;

; Descripción: Verifica que fila esta vacia en la columna seleccionada.
; Dom: board (board) X column (int) X piece (piece). 
; Rec: board.
; Tipo recursión: No aplica.

; Función para saber hasta donde llega la pieza en la columna y verificar si puede poner en esa columna.
(define (board-set-play-piece board columna piece)
  (if (equal? (cierto-element (sixth-element board) columna) 0) ; Si sexta fila tiene 0 en esa columna.
      (crear-lista (first-element board) ; Se deja igual la fila 1.
                   (second-element board) ; Se deja igual la fila 2.
                   (third-element board) ; Se deja igual la fila 3.
                   (fourth-element board) ; Se deja igual la fila 4.
                   (fifth-element board) ; Se deja igual la fila 5.
                   ; Crea nueva lista para fila 6 reemplazando el 0 con piece en la columna.
                   (crear-lista (if (= columna 0)
                                    piece (cierto-element (sixth-element board) 0)
                                    )
                                (if (= columna 1)
                                    piece (cierto-element (sixth-element board) 1)
                                    )
                                (if (= columna 2)
                                    piece (cierto-element (sixth-element board) 2)
                                    )
                                (if (= columna 3)
                                    piece (cierto-element (sixth-element board) 3)
                                    )
                                (if (= columna 4)
                                    piece (cierto-element (sixth-element board) 4)
                                    )
                                (if (= columna 5)
                                    piece (cierto-element (sixth-element board) 5)
                                    )
                                (if (= columna 6)
                                    piece (cierto-element (sixth-element board) 6)
                                    )
                                )
                   )
      
      (if (equal? (cierto-element (fifth-element board) columna) 0) ; Si quinta fila tiene 0 en esa columna.
          (crear-lista (first-element board) ; Se deja igual la fila 1.
                       (second-element board) ; Se deja igual la fila 2.
                       (third-element board) ; Se deja igual la fila 3.
                       (fourth-element board) ; Se deja igual la fila 4.
                       ; Crea nueva lista para fila 5 reemplazando el 0 con piece en la columna.
                       (crear-lista (if (= columna 0)
                                        piece (cierto-element (fifth-element board) 0)
                                        )
                                    (if (= columna 1)
                                        piece (cierto-element (fifth-element board) 1)
                                        )
                                    (if (= columna 2)
                                        piece (cierto-element (fifth-element board) 2)
                                        )
                                    (if (= columna 3)
                                        piece (cierto-element (fifth-element board) 3)
                                        )
                                    (if (= columna 4)
                                        piece (cierto-element (fifth-element board) 4)
                                        )
                                    (if (= columna 5)
                                        piece (cierto-element (fifth-element board) 5)
                                        )
                                    (if (= columna 6)
                                        piece (cierto-element (fifth-element board) 6)
                                        )
                                    )
                       (sixth-element board) ; Se deja igual la fila 6.
                       )
          
          (if (equal? (cierto-element (fourth-element board) columna) 0) ; Si cuarta fila tiene 0 en esa columna.
              (crear-lista (first-element board)   ; Se deja igual la fila 1.
                           (second-element board)   ; Se deja igual la fila 2.
                           (third-element board)    ; Se deja igual la fila 3.
                           ; Crea nueva lista para fila 4 reemplazando el 0 con piece en la columna.
                           (crear-lista (if (= columna 0) ; Si esta en esa columna se crea otra lista con el piece en ese elemento.
                                            piece (cierto-element (fourth-element board) 0)
                                            )
                                        (if (= columna 1)
                                            piece (cierto-element (fourth-element board) 1)
                                            )
                                        (if (= columna 2)
                                            piece (cierto-element (fourth-element board) 2)
                                            )
                                        (if (= columna 3)
                                            piece (cierto-element (fourth-element board) 3)
                                            )
                                        (if (= columna 4)
                                            piece (cierto-element (fourth-element board) 4)
                                            )
                                        (if (= columna 5)
                                            piece (cierto-element (fourth-element board) 5)
                                            )
                                        (if (= columna 6)
                                            piece (cierto-element (fourth-element board) 6)
                                            )
                                        )
                           (fifth-element board)    ; Se deja igual la fila 5
                           (sixth-element board))   ; Se deja igual la fila 6

              (if (equal? (cierto-element (third-element board) columna) 0) ; Si tercera fila tiene 0 en esa columna.
                  (crear-lista (first-element board)  ; Se deja igual la fila 1.
                               (second-element board)  ; Se deja igual la fila 2.
                               ; Crea nueva lista para fila 3 reemplazando el 0 con piece en la columna.
                               (crear-lista (if (= columna 0)
                                                piece (cierto-element (third-element board) 0)
                                                )
                                            (if (= columna 1)
                                                piece (cierto-element (third-element board) 1)
                                                )
                                            (if (= columna 2)
                                                piece (cierto-element (third-element board) 2)
                                                )
                                            (if (= columna 3)
                                                piece (cierto-element (third-element board) 3)
                                                )
                                            (if (= columna 4)
                                                piece (cierto-element (third-element board) 4)
                                                )
                                            (if (= columna 5)
                                                piece (cierto-element (third-element board) 5)
                                                )
                                            (if (= columna 6)
                                                piece (cierto-element (third-element board) 6)
                                                )
                                            )
                               (fourth-element board)  ; Se deja igual la fila 4.
                               (fifth-element board)  ; Se deja igual la fila 5.
                               (sixth-element board)  ; Se deja igual la fila 6.
                               )
                        
                  (if (equal? (cierto-element (second-element board) columna) 0) ; Si segunda fila tiene 0 en esa columna. 
                      (crear-lista (first-element board)  ; Se deja igual la fila 1.
                                   ; Crea nueva lista para fila 2 reemplazando el 0 con piece en la columna.
                                   (crear-lista (if (= columna 0)
                                                    piece (cierto-element (second-element board) 0)
                                                    )
                                                (if (= columna 1)
                                                    piece (cierto-element (second-element board) 1)
                                                    )
                                                (if (= columna 2)
                                                    piece (cierto-element (second-element board) 2)
                                                    )
                                                (if (= columna 3)
                                                    piece (cierto-element (second-element board) 3)
                                                    )
                                                (if (= columna 4)
                                                    piece (cierto-element (second-element board) 4)
                                                    )
                                                (if (= columna 5)
                                                    piece (cierto-element (second-element board) 5)
                                                    )
                                                (if (= columna 6)
                                                    piece (cierto-element (second-element board) 6)
                                                    )
                                                )
                                   (third-element board)  ; Se deja igual la fila 3.
                                   (fourth-element board)  ; Se deja igual la fila 4.
                                   (fifth-element board)  ; Se deja igual la fila 5.
                                   (sixth-element board)  ; Se deja igual la fila 6.
                                   )
                            
                      (if (equal? (cierto-element (first-element board) columna) 0) ; Si primera fila tiene 0 en esa columna.
                          ; Crea nueva lista para fila 1 reemplazando el 0 con piece en la columna.
                          (crear-lista (crear-lista (if (= columna 0)
                                                        piece (cierto-element (first-element board) 0)
                                                        )
                                                    (if (= columna 1)
                                                        piece (cierto-element (first-element board) 1)
                                                        )
                                                    (if (= columna 2)
                                                        piece (cierto-element (first-element board) 2)
                                                        )
                                                    (if (= columna 3)
                                                        piece (cierto-element (first-element board) 3)
                                                        )
                                                    (if (= columna 4)
                                                        piece (cierto-element (first-element board) 4)
                                                        )
                                                    (if (= columna 5)
                                                        piece (cierto-element (first-element board) 5)
                                                        )
                                                    (if (= columna 6)
                                                        piece (cierto-element (first-element board) 6)
                                                        )
                                                    )
                                       (second-element board)  ; Se deja igual la fila 2.
                                       (third-element board)  ; Se deja igual la fila 3.
                                       (fourth-element board)  ; Se deja igual la fila 4.
                                       (fifth-element board)  ; Se deja igual la fila 5.
                                       (sixth-element board)  ; Se deja igual la fila 6.
                                       )

                          (error "No hay espacio en la columna")
                          
                          )
                      )
                  )
              )
          )
      )
  )

;----------------OTROS----------------;

; Descripción: Verifica si hay al menos una celda vacía en el tablero.
; Dom: Board (board).
; Rec: Boolean (#t si se puede jugar, #f si no).
; Tipo recursión: No aplica

; Función para revisar si se puede seguir jugando, en este caso busca un espacio vacio, o sea 0.
(define (board-can-play? board)
  (if (my-member 0 (first-element board)) ; Uso IF porque member cuando encuentra el elemento entrega la cola.
      #t ; Si encuentra un 0. 
      #f ; Si no encuentra un 0.
      )
  )

;-----------------------------------------;

; Descripción: Verificar si en una columna hay 4 piezas juntas del mismo color para ganar.
; Dom: Board (board).
; Rec: int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador vertical).
; Tipo recursión: Natural.

; Función para verificar victoria en una columna.
(define (board-check-vertical-win board)
  ; Función auxiliar para revisar una columna específica.
  (define (check-columna board columna)
    (if (= columna 7)
        0 ; Si no hay win, retorna 0.
        (if (= (check-4-piezas board columna 0) 0)
            (check-columna board (+ columna 1))
            (check-4-piezas board columna 0) ; Si hay, retornamos el ganador.
            )
        )
    )

  ; Función para ver que hayan 4 fichas juntas.
  (define (check-4-piezas board columna fila)
    (if (> (+ fila 3) 5) ; Si no hay espacio para 4 en vertical.
        0
        (if (and (list? (cierto-element (cierto-element board fila) columna))
                 (list? (cierto-element (cierto-element board (+ fila 1)) columna))
                 (list? (cierto-element (cierto-element board (+ fila 2)) columna))
                 (list? (cierto-element (cierto-element board (+ fila 3)) columna)))
            (if (and (equal? (first-element (cierto-element (cierto-element board fila) columna)) 
                             (first-element (cierto-element (cierto-element board (+ fila 1)) columna))
                             )
                     (equal? (first-element (cierto-element (cierto-element board fila) columna))
                             (first-element (cierto-element (cierto-element board (+ fila 2)) columna))
                             )
                     (equal? (first-element (cierto-element (cierto-element board fila) columna))
                             (first-element (cierto-element (cierto-element board (+ fila 3)) columna))
                             )
                     )
                (if (equal? (first-element (cierto-element (cierto-element board fila) columna)) "red")
                    1 ; Retorna 1 si es rojo.
                    2 ; Retorna 2 si es amarillo.
                    )
                (if (< fila 2)
                    (check-4-piezas board columna (+ fila 1))
                    0
                    )
                )
            (if (< fila 2)
                (check-4-piezas board columna (+ fila 1))
                0
                )
            )
        )
    )

  ; Iniciamos desde la columna 0.
  (check-columna board 0)
  )


;-----------------------------------------;

; Descripción: Verificar si en una fila hay 4 piezas juntas del mismo color para ganar.
; Dom: Board (board).
; Rec: int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador horizontal).
; Tipo recursión: Natural.

; Función para revisar la victoria en una fila.
(define (board-check-horizontal-win board)
  ; Función para revisar cada fila y ver si hay win.
  (define (check-fila board contador)
    (if (= contador 6)
        0 ; Aún no se gana horizontalmente.
        (if (not (= (fila-contador board contador) 0))
            (fila-contador board contador) ; Retorna el ganador.
            (check-fila board (+ contador 1))
            )
        )
    )

  ; Función auxiliar para ver la fila a verificar.
  (define (fila-contador board contador)
    (win-or-loss (cierto-element board contador))
    )
  
  ; Función para ver si hay victoria en la fila.
  (define (win-or-loss fila)
    (if (< (largo fila) 4)
        0  ; Si tiene menos de 4 piezas, no hay victoria horizontal.
        (check-4-piezas fila 0)
        )
    )
  
  ; Función para verificar 4 piezas iguales en una fila.
  (define (check-4-piezas fila posicion)
    (if (> (+ posicion 3) (- (largo fila) 1))
        0 ; No hay 4 piezas juntas.
        (if (and (list? (cierto-element fila posicion))
                 (list? (cierto-element fila (+ posicion 1)))
                 (list? (cierto-element fila (+ posicion 2)))
                 (list? (cierto-element fila (+ posicion 3)))
                 (equal? (cierto-element fila posicion) (cierto-element fila (+ posicion 1)))
                 (equal? (cierto-element fila posicion) (cierto-element fila (+ posicion 2)))
                 (equal? (cierto-element fila posicion) (cierto-element fila (+ posicion 3)))
                 (not (equal? (cierto-element fila posicion) 0))) ; Que no sea 0.
            (if (equal? (first-element (cierto-element fila posicion)) "red")
                1 ; Retorna 1 si es rojo, 2 si es amarillo.
                2
                )
            (check-4-piezas fila (+ posicion 1))
            )
        
        )
    )
  
  ; Iniciamos chequeando desde la fila 0.
  (check-fila board 0)
  )
 
;-----------------------------------------;

; Descripción: Verificar si en una diagonal hay 4 piezas juntas del mismo color para ganar.
; Dom: Board (board).
; Rec: int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador diagonal).
; Tipo recursión: Cola.

; Función para verificar victoria diagonal
(define (board-check-diagonal-win board)
  ; Función principal para revisar diagonales
  (define (check-diagonales board columna fila direccion)
    (cond 
      ; Casos base.
      [(= columna 4) 0]  ; Llegamos a la ultima columna y no hay.
      [(and (= direccion 1) (= fila 3))(check-diagonales board (+ columna 1) 0 1)]  
      [(and (= direccion 2) (= fila 6))(check-diagonales board (+ columna 1) 3 2)] ; Siguiente columna ascendente
      
      ; Casos recursivos. Se ve el caso actual.
      [(and (= direccion 1)(not (= (check-4-piezas-descendente board columna fila) 0)))(check-4-piezas-descendente board columna fila)]
      [(and (= direccion 2)(not (= (check-4-piezas-ascendente board columna fila) 0)))(check-4-piezas-ascendente board columna fila)]
      
      ; Si el caso actual no tiene vistoria, seguimos con los otros.
      [(= fila 0)(+ (check-diagonales board columna (+ fila 1) 1)(check-diagonales board columna 3 2))]
      [else (check-diagonales board columna (+ fila 1) direccion)]
      )
    )

  ; Función para ver diagonal descendente.
  (define (check-4-piezas-descendente board columna fila)
    (if (or (> (+ columna 3) 6) (> (+ fila 3) 5))  ; Ver cual es el final del tablero.
        0
        (if (and (list? (cierto-element (cierto-element board fila) columna)) ; Comprobar que son listas.
                 (list? (cierto-element (cierto-element board (+ fila 1)) (+ columna 1)))
                 (list? (cierto-element (cierto-element board (+ fila 2)) (+ columna 2)))
                 (list? (cierto-element (cierto-element board (+ fila 3)) (+ columna 3))))
            (if (and (equal? (first-element (cierto-element (cierto-element board fila) columna))
                             (first-element (cierto-element (cierto-element board (+ fila 1)) (+ columna 1))))
                     (equal? (first-element (cierto-element (cierto-element board fila) columna))
                             (first-element (cierto-element (cierto-element board (+ fila 2)) (+ columna 2))))
                     (equal? (first-element (cierto-element (cierto-element board fila) columna))
                             (first-element (cierto-element (cierto-element board (+ fila 3)) (+ columna 3)))))
                (if (equal? (first-element (cierto-element (cierto-element board fila) columna)) "red")
                    1  ; Gana rojo.
                    2 ; Gana amarillo.
                    )
                0
                )
            0
            )
        )
    )
  
  ; Función para ver diagonal ascendente.
  (define (check-4-piezas-ascendente board columna fila)
    (if (or (> (+ columna 3) 6) (< (- fila 3) 0))  ; Ver cual es el final del tablero.
        0
        (if (and (list? (cierto-element (cierto-element board fila) columna)) 
                 (list? (cierto-element (cierto-element board (- fila 1)) (+ columna 1)))
                 (list? (cierto-element (cierto-element board (- fila 2)) (+ columna 2)))
                 (list? (cierto-element (cierto-element board (- fila 3)) (+ columna 3))))
            (if (and (equal? (first-element (cierto-element (cierto-element board fila) columna))
                            (first-element (cierto-element (cierto-element board (- fila 1)) (+ columna 1))))
                     (equal? (first-element (cierto-element (cierto-element board fila) columna))
                            (first-element (cierto-element (cierto-element board (- fila 2)) (+ columna 2))))
                     (equal? (first-element (cierto-element (cierto-element board fila) columna))
                            (first-element (cierto-element (cierto-element board (- fila 3)) (+ columna 3)))))
                (if (equal? (first-element (cierto-element (cierto-element board fila) columna)) "red")
                    1  ; Gana rojo.
                    2 ; Gana amarillo.
                    )
                0
                )
            0
            )
        )
    )

  ; Verificar diagonales desde columna 0 y fila 0.
  (check-diagonales board 0 0 1)
  )

;-----------------------------------------;

; Descripción: Verifica el estado del tablero y entrega el posible ganador.
; Dom: Board (board).
; Rec: int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador).
; Tipo recursión: No aplica.

; Función que verifica en todas las formas de ganar cual es el ganador.
(define (board-who-is-winner board)
  (define resultado-en-vertical (board-check-vertical-win board))
  (define resultado-en-horizontal (board-check-horizontal-win board))
  (define resultado-en-diagonal (board-check-diagonal-win board))
  
  (cond
    [(> resultado-en-vertical 0) resultado-en-vertical]    ; Retorna el ganador vertical.
    [(> resultado-en-horizontal 0) resultado-en-horizontal] ; Retorna el ganador horizontal.
    [(> resultado-en-diagonal 0) resultado-en-diagonal]     ; Retorna el ganador diagonal.
    [else 0] ; No hay ganador.
    )
  )  

;-----------------------------------------;






