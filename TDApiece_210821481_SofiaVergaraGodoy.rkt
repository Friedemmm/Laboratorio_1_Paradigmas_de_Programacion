#lang racket

(provide (all-defined-out))

(require "TDAplayer_2108221481_SofiaVergaraGodoy.rkt")

;----------------CONSTRUCTOR-PIECE----------------;

; Descripción: Crea una ficha, validando que sea string para luego transformarlo en lista.
; Dom: Color (string).
; Rec: La pieza creada (piece).
; Tipo recursión: No aplica.

; Función para crear una ficha.
(define (piece color)
  (if (not (string? color))
      (error "color debe ser una cadena de caracteres")
      (crear-lista color)
      )
  )

;-----------------------------------------;
