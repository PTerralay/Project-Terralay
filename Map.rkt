#lang racket

(provide Map% Load&Create)

(require "Tile.rkt")

(define Map%
  (class object%
    (super-new)
    (init-field sizex
                sizey
                tiles)
    (field (agentlist (mcons '() '())))
    (define/public (gettile gridx gridy) 
      (vector-ref (vector-ref tiles gridy) gridx))
    (define/public (render) 
      "not implemented yet!")
    (define/public (get-sizex)
      sizex)
    (define/public (get-sizey)
      sizey)
    (define/public (Add-agent! agent)
      (set! agentlist (mcons agent agentlist)))
    (define/public (get-tile-vector)
      tiles)
    ))


;Edited to fix a bug with line-ends when running on linux. The process now properly 
;checks for all legitimate combinations of new-line characters: \n, \r and \r\n 
;(\n\r is not used by any interesting platforms)
(define (map-load map-file)
  (let ((y-vector '())
        (data-file (open-input-file map-file)))    
    (define (y-loop)
      (let ((x-vector '()))
        (define (x-loop)
          (let ((data (read-char data-file)))
            (when (and (eq? data #\return) 
                       (eq? (peek-char data-file) #\newline))
              (read-char data-file)); If the sequence \r\n is encountered, the reader is simply incremented
            (if (or (eq? data #\return) (eq? data #\newline))
                (list->vector (reverse x-vector))
                (begin (set! x-vector (cons (new Tile% (type data)) x-vector))
                       (x-loop)))))
        (let ((vector-candidate (x-loop)))
          (set! y-vector (cons vector-candidate y-vector))
          (if (eof-object? (peek-char data-file))
              (begin (close-input-port data-file)
                     (list->vector (reverse y-vector)))
              (y-loop)))))
    (y-loop)))

(define (Load&Create mapname filename)
  (let ((tilemap (map-load filename)))
    (new Map% (sizex (vector-length (vector-ref tilemap 0))) (sizey (vector-length tilemap)) (tiles tilemap))))

