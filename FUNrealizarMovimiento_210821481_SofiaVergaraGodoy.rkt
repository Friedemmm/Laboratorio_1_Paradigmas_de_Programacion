; Descripción: Verifica que sea turno del jugador, actualia el estado del juego después de un movimiento, veritidca el ganador o empate y si terminó, actualiza las estadísticas.
; Dom: game (game) X player (player) X column (int).
; Rec: Game.
; Tipo recursión: No aplica.

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAgameSetEnd_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAturno_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAjugarFicha_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAhistorial_2108221481_SofiaVergaraGodoy.rkt")

; Función auxiliar para restar fichas.
(define (fichas-menos player)
  (player (first-element player) ; ID
          (second-element player) ; Nombre.
          (third-element player) ; Color.
          (fourth-element player) ; Victorias.
          (fifth-element player) ; Derrotas.
          (sixth-element player) ; Empates.
          (-1 (seventh-element player)) ; Fichas - 1.
          )
  )
; Función auxiliar aumentar turnos.
(define (actualizar-turno game) ; Lista Game: player1 player2 board turno.
  (game (first-element game)
        (second-element game)
        (third-element game)
        (+1 (fourth-element game))
        )
  )
  

; Función principal que realiza el movimiento
(define (game-player-set-move game player columna) ;  Lista Game: player1 player2 board turno. 
  (if (game-set-end game)
      (error "el juego ya ha terminado, no puede jugar")
      (if (not (equal? (game-get-current-player game) player))
          (error "le toca al otro jugador")
          (if (board-set-play-piece (third-element game) player columna)
              (if (fichas-menos player)
                  (if (game-history game)
                      (game-set-end game)
                      "error al actualizar historial"
                      )
                  "error al actualizar fichas"
                  )
              "error al colocar pieza"
              )
          )
      )
  )
