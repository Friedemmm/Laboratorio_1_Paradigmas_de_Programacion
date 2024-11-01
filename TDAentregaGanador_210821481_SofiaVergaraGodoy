; Descripción: Verifica el estado del tablero y entrega el posible ganador.
; Dom: Board (board).
; Rec: int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador).
; Tipo recursión: No aplica.

(require "TDAboardWinDiagonal_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAboardWinHorizontal_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAboardWinVertical_2108221481_SofiaVergaraGodoy.rkt")

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
 
