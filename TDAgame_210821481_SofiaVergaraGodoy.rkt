#lang racket

(provide (all-defined-out))

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")
(require "TDApiece_2108221481_SofiaVergaraGodoy.rkt")
(require "TDAboard_2108221481_SofiaVergaraGodoy.rkt")

;----------------CONSTRUCTOR-GAME----------------;

; Descripción: Crear una partida.
; Dom: player1 (player) X player2 (player) X board (board) X current-turn (int).
; Rec: Game.
; Tipo recursión: No aplica.

(define (game player1 player2 board turno)
  (crear-lista player1 player2 board turno)
  )

;----------------SELECTORES----------------;

; Descripción: Obtiene el jugador cuyo turno esta en curso.
; Dom: Game (game).
; Rec: Player.
; Tipo recursión: No aplica.

(define (game-get-current-player game) ; Lista Game: player1 player2 board turno. ; Lista Player: id name color wins losses draws remaining-pieces.
  (if (= (modulo (fourth-element game) 2) 1)
      (first-element game) ; Si es impar, le toca a player1.
      (second-element game) ; Si es par, le toca a player2.
      )
  )

;-----------------------------------------;

; Descripción: Entregar el estado actual del tablero.
; Dom: Game (game).
; Rec: Board.
; Tipo recursión: No aplica.

; Función para printear el tablero.
(define (game-get-board game) ; Lista Game: player1 player2 board turno. 
  (newline)
  (third-element game)
  )

;---------------MODIFICADORES-----------------;

; Descripción: Se finaliza el juego y actualiza las estadisticas de los jugadores según el resultado.|
; Dom: Game (game).
; Rec: Game.
; Tipo recursión: No aplica.

; Función que dependiendo del resultado llama a actualizar las stats.
(define (game-set-end game)
  (cond
    ; Empate
    [(equal? (game-is-draw? game) #t)
     (list
       (player-update-stats (first-element game) "draw")
       (player-update-stats (second-element game) "draw")
       (third-element game)
       (fourth-element game))]
    
    ; Player1 gana
    [(= (board-who-is-winner (third-element game)) 1)
     (list
       (player-update-stats (first-element game) "win")
       (player-update-stats (second-element game) "loss")
       (third-element game)
       (fourth-element game))]
    
    ; Player2 gana
    [(= (board-who-is-winner (third-element game)) 2)
     (list
       (player-update-stats (first-element game) "loss")
       (player-update-stats (second-element game) "win")
       (third-element game)
       (fourth-element game))]
    
    ; No ha terminado
    [else game]
    )
  )

;-----------------------------------------;

; Descripción: Verifica que sea turno del jugador, actualia el estado del juego después de un movimiento, verifica el ganador o empate y si terminó, actualiza las estadísticas.
; Dom: game (game) X player (player) X column (int).
; Rec: Game.
; Tipo recursión: No aplica.

; Función principal para mover y hacer todo lo demás.
(define (game-player-set-move game player columna)
  (cond
    [(game-ended? game) (error "El juego ya ha terminado")] ; Verifica si el juego ya terminó antes de jugar.
    
    [(not (= (first-element player)(first-element (game-get-current-player game))))(error "No es tu turno")]
    
    [(= (seventh-element (if (= (first-element player) 1)(first-element game)(second-element game))) 0)(error "No tienes fichas disponibles")] ; Revisamos las fichas del player.
    
    [else (empate-o-otro
           (crear-lista
            (actualizar-jugador 1 player game
                                (crear-lista (first-element player)
                                             (second-element player)
                                             (third-element player)
                                             (fourth-element player)
                                             (fifth-element player)
                                             (sixth-element player)
                                             (- (seventh-element (if (= (first-element player) 1)
                                                                     (first-element game)
                                                                     (second-element game)
                                                                     )) 1)
                                             )
                                )
            (actualizar-jugador 2 player game 
                                (crear-lista (first-element player)
                                             (second-element player)
                                             (third-element player)
                                             (fourth-element player)
                                             (fifth-element player)
                                             (sixth-element player)
                                             (- (seventh-element (if (= (first-element player) 2)
                                                                     (second-element game)
                                                                     (first-element game)
                                                                     )) 1)
                                             )
                                )
            (board-set-play-piece 
             (third-element game)
             columna
             (piece (third-element player))
             )
            (+ (fourth-element game) 1)
            )
           )
          ]
    )
  )

; Función auxiliar para actualizar al plater.
(define (actualizar-jugador id current-player game updated-player)
  (if (= (first-element current-player) id)
      updated-player
      (if (= id 1)
          (first-element game) ; Usamos el player actual.
          (second-element game)
          )
      )
  ) 

; Función auxliar para verificar que termino el juego.
(define (game-ended? game)
  (or (not (= (board-who-is-winner (third-element game)) 0))
      (game-is-draw? game)))

; Función auxliar para verificar que empataron o gano alguien.
(define (empate-o-otro new-game)
  (cond
    [(not (= (board-who-is-winner (third-element new-game)) 0))(game-set-end new-game)]
    [(game-is-draw? new-game)(game-set-end new-game)]
    [else new-game]
    )
  )

;-----------------OTROS---------------------;

; Descripción: Generar un historial de movimientos de la partida.
; Dom: Game (game).
; Rec: Lista de movimientos. Cada movimiento es un par (columna . color).
; Tipo recursión: No aplica.

; Función auxiliar para obtener el color de una posición del board.
(define (color-posicion board fila columna)
  (if (list? (cierto-element (cierto-element board fila) columna))
      (first-element (cierto-element (cierto-element board fila) columna))
      "empty"
      )
  )

; Función auxiliar los mov en el board.
(define (find-mov board initial-board fila columna acc)
  (cond
    [(>= fila 6) acc] ; Terminamos de recorrer.
    [(>= col 7) (find-mov board initial-board (+ fila 1) 0 acc)] ; Siguiente fila.
    [(not (equal? (color-posicion board fila columna)(color-posicion initial-board fila columna))) 
     (find-mov board initial-board fila (+ columna 1)(my-cons (crear-lista fila columna (color-posicion board fila columna)) acc))
     ]
    [else (find-mov board initial-board fila (+ columna 1) acc)] ; Siguiente columna.
    )
  )    

; Función para asignar turnos a los movimientos encontrados.
(define (asignar-turnos mov turno)
  (if (null? mov)
      '()
      (my-cons (crear-lista turno
                            (second-element (my-car mov))
                            (third-element (my-car mov))
                            )
               (asignar-turnos (my-cdr mov) (- turno 1))
               )
      )
  )

; Función para construir el historial.
(define (game-history game)
  (if (equal? (third-element game) (board))
      '()
      (reverse
       (asignar-turnos 
        (find-mov (third-element game) (board) 0 0 '())
        (- (fourth-element game) 1)
        )
       )
      )
  )

;----------------------------------------;

; Descripción: Verificar si los jugadores se quedaron sin fichas o no gano nadie y no queda espacio en el tablero.
; Dom: Game (game).
; Rec: boolean (#t si es empate, #f si no).
; Tipo recursión: No aplica.

; Función para verificar empate.
(define (game-is-draw? game) ; Lista Game: player1 player2 board turno. ; Lista Player: id name color wins losses draws remaining-pieces.
  (cond 
    [(equal? (board-can-play? (third-element game)) #f) #t] ; Tablero lleno
    [(and (= (seventh-element (first-element game)) 0)(= (seventh-element (second-element game)) 0)) #t] ; Si ambos tienen 0 fichas.          
    [else #f] ; No es empate.
    )
  )

;-----------------------------------------;

; Descripción: Actulizar las estadisticas del jugador, ya sea victoria, derrota o empate.
; Dom: player (player) X result (string: "win", "loss", o "draw").
; Rec: player
; Tipo recursión: No aplica.

; Función para agregar +1 dependiendo de el resultado.
(define (player-update-stats player result)
  (cond ; Lista Player: id name color wins losses draws remaining-pieces.
    [(equal? result "win") 
     (crear-lista (first-element player) ; ID
             (second-element player) ; Nombre.
             (third-element player) ; Color.
             (+ (fourth-element player) 1) ; Victorias + 1.
             (fifth-element player) ; Derrotas.
             (sixth-element player) ; Empates.
             (seventh-element player) ; Fichas.
             )
     ]
    
    [(equal? result "loss") 
     (crear-lista (first-element player) ; ID.
                 (second-element player) ; Nombre.
                 (third-element player) ; Color.
                 (fourth-element player) ; Victorias.
                 (+ (fifth-element player) 1) ; Derrotas + 1.
                 (sixth-element player) ; Empates.
                 (seventh-element player) ; Fichas.
                 )
     ]
  
    
    [(equal? result "draw")
     (crear-lista (first-element player) ; ID.
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
;-----------------------------------------;









