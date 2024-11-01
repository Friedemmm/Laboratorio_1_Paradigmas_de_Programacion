; Descripción: Crear una partida.
; Dom: player1 (player) X player2 (player) X board (board) X current-turn (int).
; Rec: Game.
; Tipo recursión: No aplica.

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")

(define (game player1 player2 board turno)
  (crear-lista player1 player2 board turno)
  )

-------Funciones Auxiliares-----

; Descripción: Generar un historial de movimientos de la partida.
; Dom: Game (game).
; Rec: Lista de movimientos. Cada movimiento es un par (columna . color).
; Tipo recursión: No aplica.

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

; Descripción: Verificar si los jugadores se quedaron sin fichas o no gano nadie y no queda espacio en el tablero.
; Dom: Game (game).
; Rec: boolean (#t si es empate, #f si no).
; Tipo recursión: No aplica.

; Función para verificar empate.
(define (game-is-draw? game) ; Lista Game: player1 player2 board turno. ; Lista Player: id name color wins losses draws remaining-pieces.
  (cond 
    [(= (board-can-play? (third-element game)) #f) #t] ; Tablero lleno
    [(and (= (seventh-element (first-element game)) 0)(= (seventh-element (second-element game)) 0)) #t] ; Si ambos tienen 0 fichas.          
    [else #f] ; No es empate.
    )
  ) 

; Descripción: Actulizar las estadisticas del jugador, ya sea victoria, derrota o empate.
; Dom: player (player) X result (string: "win", "loss", o "draw").
; Rec: player.
; Tipo recursión: No aplica.

; Función para agregar +1 a la estadistica que termine el juego.
(define (player-update-stats player result)
  (cond ; Lista Player: id name color wins losses draws remaining-pieces.
    [(equal? result "win") 
     (player (first-element player) ; ID
             (second-element player) ; Nombre.
             (third-element player) ; Color.
             (+ (fourth-element player) 1) ; Victorias + 1.
             (fifth-element player) ; Derrotas.
             (sixth-element player) ; Empates.
             (seventh-element player) ; Fichas.
             )
     ]
    
    [(equal? result "loss") 
     (player (first-element player) ; ID.
             (second-element player) ; Nombre.
             (third-element player) ; Color.
             (fourth-element player) ; Victorias.
             (+ (fifth-element player) 1) ; Derrotas + 1.
             (sixth-element player) ; Empates.
             (seventh-element player) ; Fichas.
             )
     ]
  
    
    [(equal? result "draw")
     (player (first-element player) ; ID.
             (second-element player) ; Nombre.
             (third-element player) ; Color.
             (fourth-element player) ; Victorias.
             (fifth-element player) ; Derrotas.
             (+ (sixth-element player) 1) ; Empates + 1.
             (seventh-element player) ; Fichas.
             )
     ]
    
    [else player]
    )
  )       

; Descripción: Obtiene el jugador cuyo turno esta en curso.
; Dom: Game (game).
; Rec: Player.
; Tipo recursión: No aplica.

(define (game-get-current-player game) ; Lista Game: player1 player2 board turno. ; Lista Player: id name color wins losses draws remaining-pieces.
  (cond
    [(> (seventh-element (first-element game))(seventh-element (second-element game))) first-element] ; Si player1 > player2 en fichas, le toca al player1.
    [(< (seventh-element (first-element game))(seventh-element (second-element game))) second-element] ; Si player1 < player2 en fichas, le toca al player2.
    [(= (seventh-element (first-element game))(seventh-element (second-element game))) first-element] ; Si tienen igual fichas, empieza player1.
    [else game]
    )
  )

; Descripción: Entregar el estado actual del tablero.
; Dom: Game (game).
; Rec: Board.
; Tipo recursión: No aplica.

; Función para printear el tablero.
(define (game-get-board game) ; Lista Game: player1 player2 board turno. 
  (print "Estado actual del tablero:")
  (print (third-element game))
  (third-element game)
  )

; Descripción: Se finaliza el juego y actualiza las estadisticas de los jugadores según el resultado.
; Dom: Game (game).
; Rec: Game.
; Tipo recursión: No aplica.

; Función que dependiendo del resultado llama a actualizar las stats.
(define (game-set-end game)
  (cond
    ; Empate.
    [(game-is-draw? game)
     (player-update-stats (first-element game) "draw")
     (player-update-stats (second-element game) "draw")
     ]

    ; Player1 gana.
    [(= (board-who-is-winner (third-element game)) 1)
     (player-update-stats (first-element game) "win")
     (player-update-stats (second-element game) "loss")
     ]

    ; Player2 gana.
    [(= (board-who-is-winner (third-element game)) 2)
     (player-update-stats (first-element game) "loss")
     (player-update-stats (second-element game) "win")
     ]
    )
  )

; Descripción: Verifica que sea turno del jugador, actualia el estado del juego después de un movimiento, veritidca el ganador o empate y si terminó, actualiza las estadísticas.
; Dom: game (game) X player (player) X column (int).
; Rec: Game.
; Tipo recursión: No aplica.

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
