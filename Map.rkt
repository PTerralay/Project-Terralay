#lang racket

(provide Map% load&create-map)

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


(define (map-load filename)
  (let ((y-vector '())
        (data-file (open-input-file filename)))    
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


(define (load&create-map mapname filename)
  (parameterize ((current-namespace (make-base-namespace))) ;Needed to avoid the otherwise empty namespace when calling load&create from top-level in game.rkt
    (let* ((data (load/cd filename))
           (tilemap (map-load (cdr (assq 'mapfile data)))))
      (new Map% 
           (sizex (vector-length (vector-ref tilemap 0)))
           (sizey (vector-length tilemap))
           (tiles tilemap)))))

;(send map Load&Create 'torsk "testAI.txt")
