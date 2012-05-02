#lang racket

(provide Map% load&create-map)

(require "Tile.rkt" "Trigger.rkt" "Character.rkt" "Thing.rkt"
         racket/mpair)

;------------------------------------------------------------------------------
;Class: Map%
;Description: A "room" in the game, a closed area of the world.
;------------------------------------------------------------------------------
(define Map%
  (class object%
    (super-new)
    (init-field sizex
                sizey
                tile-vectors
                mapID
                neighbours)
    
    ;------------------------------------------------------------------------------
    ;gettile: returns the tile object on the specified position
    ;params:
    ; gridx - the x coord.
    ; gridy - the y coord.
    ;------------------------------------------------------------------------------
    (define/public (gettile gridx gridy) 
      (vector-ref (vector-ref tile-vectors gridy) gridx))))
    
   

;------------------------------------------------------------------------------
;map-load: Loads the tile data into a new tile vector from a map file.
;params: 
; filename - the name of the file containing the tile layout
; triggers - a list of the triggers to be put in the correct tiles
;------------------------------------------------------------------------------
(define (map-load filename triggers)
  (let ((iy 0)
        (y-vector '())
        (data-file (open-input-file filename)))    
    (define (y-loop)
      (let ((ix 0)
            (x-vector '()))
        (define (x-loop)
          (let ((data (read-char data-file)))
            (when (and (eq? data #\return)
                       (eq? (peek-char data-file) #\newline))
              (read-char data-file)); If the sequence \r\n is encountered, the reader is simply incremented
            (if (or (eq? data #\return) (eq? data #\newline))
                (list->vector (reverse x-vector))
                (begin
                  (let ((tile-candidate (new Tile% (gridx ix) (gridy iy) (type data))))
                    (for-each (lambda (trigger-data)       
                                (when (and (eq? (cdr (assq 'x trigger-data)) ix) (eq? (cdr (assq 'y trigger-data)) iy))
                                  (send tile-candidate add-trigger! (new Trigger% (trigger-assoc trigger-data)))))
                              triggers)
                    (set! x-vector (cons tile-candidate x-vector)))
                  (set! ix (+ ix 1))
                  (x-loop)))))
        (let ((vector-candidate (x-loop)))
          (set! y-vector (cons vector-candidate y-vector))
          (set! iy (+ iy 1))
          (if (eof-object? (peek-char data-file))
              (begin (close-input-port data-file)
                     (list->vector (reverse y-vector)))
              (y-loop)))))
    (y-loop)))


;------------------------------------------------------------------------------
;load&create-map: Loads an entire map from a config file.
;params: 
; mapname - the ID of the new map
; filename - the name of the file containing the map config data
; world - the entire world in the game
;------------------------------------------------------------------------------
(define (load&create-map mapname filename world)
  (let* ((mapfile (dynamic-require filename 'mapfile))
         (triggers (dynamic-require filename 'triggers))
         (tilemap (map-load mapfile triggers))
         (neighbourlist (dynamic-require filename 'neighbours))
         (map-candidate (new Map% (sizex (vector-length (vector-ref tilemap 0)))
                             (sizey (vector-length tilemap))
                             (tile-vectors tilemap)
                             (neighbours neighbourlist)
                             (mapID mapname))))
    map-candidate))