#lang racket
(provide Index)
;-------------------------------------------------------------------------------
;this is just an index for the maps that are to be included in the game,
;if the map isn't in this list it doesn't exist in the game.
;-------------------------------------------------------------------------------
(define Index
  (list
   (cons 'Awesomeroom "maps/Awesomeroom.stuff")
   (cons 'Anotherawesomeroom "maps/Anotherawesomeroom.stuff")
   (cons 'Workroom "maps/Workroom.stuff")
   (cons 'Engineroom1 "maps/Engineroom1.stuff")
   (cons 'Stairs1 "maps/Stairs1.stuff")
   (cons 'outside_workroom "maps/outside_workroom.stuff")
   (cons 'Office1 "maps/Office1.stuff")
   (cons 'Relayroom "maps/Relayroom.stuff")))