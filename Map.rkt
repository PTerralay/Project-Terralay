#lang racket

(provide Map%)

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
      "not implemented yet!")))



(define (map-load map-file)
  (let ((y-vector '())
        (data-file (open-input-file map-file)))
    
    (define (y-loop)
      (let ((x-vector '()))
        
        (define (x-loop)
          (let ((data (read-char data-file)))
            ;(display data)
            (if (or (eq? data #\newline) (eq? data #\return))
                (list->vector (reverse x-vector)) ;change so that the entire Tile-object is insertet to the list rather than the letter corresponding to it.
                (begin (set! x-vector (cons (new Tile% (type data)) x-vector))
                       (x-loop))))) ;Creates a list with every character on a line from a file as elements
        
        (let ((vector-candidate (x-loop)))
          (if (eof-object? (peek-char data-file)) ;check if we are at end of file
              (begin (close-input-port data-file)
                     (list->vector (reverse y-vector)))
              (if (= (vector-length vector-candidate) 0)
                  (y-loop)
                  (begin (set! y-vector (cons vector-candidate y-vector))
                         (y-loop)))))))
    (y-loop))
  (new Map% (sizex ) (sizey y)