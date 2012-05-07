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
    ; adds the trigger to triggerlist
    ;------------------------------------------------------------------------------------
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))
    
    ;------------------------------------------------------------------------------------
    ;sets passable to false if tile is a unpassable type, else it is set to be true
    ;------------------------------------------------------------------------------------
    (set! passable
          (case type
            ((0) #t)
            (else #f)))))