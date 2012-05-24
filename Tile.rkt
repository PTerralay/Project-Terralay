#lang racket

(provide Tile%)


;-------------------------------------------------------------------------------
;class: object
;desc:
; The tiles that makes up the room, these are either passable or not,
; they can also contain triggers
;-------------------------------------------------------------------------------
(define Tile%
  (class object%
    (super-new)
    (init-field type
                gridx
                gridy
                texfamily
                textype)
    
    (field (passable #t))
    
    ;sets passable to true only if the tile is of a floor type
    (set! passable
          (case texfamily
            ((0 2) #t)
            (else #f)))))