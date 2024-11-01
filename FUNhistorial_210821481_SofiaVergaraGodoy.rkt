; Descripción: Generar un historial de movimientos de la partida.
; Dom: Game (game).
; Rec: Lista de movimientos. Cada movimiento es un par (columna . color).
; Tipo recursión: No aplica.

(require "TDAplayer_2108221481_SofiaVergara.rkt")

; Función para encontrar una pieza en el tablero
(define (find-piece board fila columna)
  (if (list? (cierto-element (cierto-element board fila) columna))
      (first-element (cierto-element (cierto-element board fila) columna))
      0)
)

; Función para encontrar la fila donde cayó la pieza.
(define (find-fila board columna)
  (define (aux-find-fila fila)
    (cond 
      [(= fila 6) -1]  ; No se encontró fila (error).
      [(list? (cierto-element (cierto-element board fila) columna)) fila]
      [else (aux-find-fila (+ fila 1))]
      )
    )
  (aux-find-fila 0)
  )

; Función auxiliar para crear una entrada en el historial
(define (create-history-entry board turno columna fila)
  (list turno columna (find-piece board fila columna)
        )
  )

; Función auxiliar para procesar una jugada para el historial
(define (process-history-move board turno columna)
  (define fila (find-fila board columna))
  (if (= fila -1)
      '() ; Error: no se encontró la última jugada
      (cons (create-history-entry board turno columna fila)
            (game-history (board) (- turno 1))
            )
      )
  )

; Función principal para construir el historial de jugadas.
(define (game-history board turno)
  (if (equal? board (board))  ; Si es tablero vacío.
      '()  ; Retorna lista vacía.
      (process-history-move board turno (- (fourth-element game) 1))
      )
  )

; Iniciar construcción del historial con el tablero actual.
(define (get-game-history game)
  (game-history (third-element game) (fourth-element game))
  )
