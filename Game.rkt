;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;
#lang racket/gui


(require "world.rkt" "graphics.rkt" "player.rkt")


(define world (new World% (maplist '()) (current-map #f) (state 0)))

(define (tick)
  (send glcanvas refresh))


(new timer% (interval 500) (notify-callback tick))


(send frame show #t)