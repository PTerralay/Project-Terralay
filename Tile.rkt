#lang racket

(provide Tile%)


;------------------------------------------------------------------------------------
;class: object
;desc: the tiles that makes up the room, these are either passable or not, they can also contain triggers
;------------------------------------------------------------------------------------
(define Tile%
  (class object%
    (super-new)
    (init-field type
                gridx
                gridy)
    
    (field (passable #t)
           (triggerlist '())
           (tilebackround "fucked if I know"))
    
    ;------------------------------------------------------------------------------------
    ;add-trigger!: adds a trigger to triggerlist'
    ;params:
    ; trigger - a trigger object
    ;------------------------------------------------------------------------------------
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))
    
    ;sets passable to true only if the tile is of a floor type
    (set! passable
          (case type
            ((0 17) #t)
            (else #f)))))