; Descripción: Verificar si los jugadores se quedaron sin fichas o no gano nadie y no queda espacio en el tablero.
; Dom: Game (game).
; Rec: boolean (#t si es empate, #f si no).
; Tipo recursión: No aplica.

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAsePuedeJugar?_2108221481_SofiaVergaraGodoy.rkt")

; Función para verificar empate.
(define (game-is-draw? game) ; Lista Game: player1 player2 board turno. ; Lista Player: id name color wins losses draws remaining-pieces.
  (cond 
    [(= (board-can-play? (third-element game)) #f) #t] ; Tablero lleno
    [(and (= (seventh-element (first-element game)) 0)(= (seventh-element (second-element game)) 0)) #t] ; Si ambos tienen 0 fichas.          
    [else #f] ; No es empate.
    )
  ) 
