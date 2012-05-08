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
(define (map-load data-file triggers) ;Don't forget that maps are loaded both in the initial load&create-map in Game, but also when setting the current map and loading the neighbours
  (let ((iy 0)
        (y-vector '()))    
    (define (y-loop)
      (let ((ix 0)
            (x-vector '()))
        (define (x-loop)
          (let ((data1 (read-char data-file)))
            (when (and (eq? data1 #\return)
                       (eq? (peek-char data-file) #\newline))
              (read-char data-file)); If the sequence \r\n is encountered, the reader is simply incremented
            (if (or (eq? data1 #\return) (eq? data1 #\newline) (eof-object? data1))
                (list->vector (reverse x-vector))
                (let* ((data2 (read-char data-file))
                       (data3 (read-char data-file))
                       (type-number (+ (* (string->number (string data1)) 100)
                                                     (* (string->number (string data2)) 10)
                                                     (string->number (string data3))))
                       (tile-candidate (new Tile% (gridx ix)
                                            (gridy iy)
                                            (type type-number)
                                            (texfamily (quotient type-number 16))
                                            (textype (remainder type-number 16)))))
                  (for-each (lambda (trigger-data)       
                              (when (and (eq? (cdr (assq 'x trigger-data)) ix) (eq? (cdr (assq 'y trigger-data)) iy))
                                (send tile-candidate add-trigger! (new Trigger% (trigger-assoc trigger-data)))))
                            triggers)
                  (when (eq? (get-field type tile-candidate) 999)
                    (set-field! type tile-candidate #f))
                  (set! x-vector (cons tile-candidate x-vector))
                  (set! ix (+ ix 1))
                  (x-loop)))))
        (let ((vector-candidate (x-loop)))
          (unless (null? vector-candidate)
            (set! y-vector (cons vector-candidate y-vector))
            (set! iy (+ iy 1)))
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
         (tilemap (map-load (open-input-file mapfile #:mode 'text) triggers))
         (neighbourlist (dynamic-require filename 'neighbours))
         (map-candidate (new Map% (sizex (vector-length (vector-ref tilemap 0)))
                             (sizey (vector-length tilemap))
                             (tile-vectors tilemap)
                             (neighbours neighbourlist)
                             (mapID mapname))))
    (display "I MADE A MAP! : ")
    (display tilemap)
    map-candidate))