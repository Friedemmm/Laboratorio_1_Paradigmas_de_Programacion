; Descripción: Obtiene el jugador cuyo turno esta en curso.
; Dom: Game (game).
; Rec: Player.
; Tipo recursión: No aplica.

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")

(define (game-get-current-player game) ; Lista Game: player1 player2 board turno. ; Lista Player: id name color wins losses draws remaining-pieces.
  (cond
    [(> (seventh-element (first-element game))(seventh-element (second-element game))) first-element] ; Si player1 > player2 en fichas, le toca al player1.
    [(< (seventh-element (first-element game))(seventh-element (second-element game))) second-element] ; Si player1 < player2 en fichas, le toca al player2.
    [(= (seventh-element (first-element game))(seventh-element (second-element game))) first-element] ; Si tienen igual fichas, empieza player1.
    [else game]
    )
  )
