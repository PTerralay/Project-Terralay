#lang racket

(provide Map% load&create-map)

(require "Tile.rkt" "Trigger.rkt")

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
                  (let ((tile-candidate (new Tile% (x ix) (y iy) (type data))))
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


(define (load&create-map mapname filename)
  ;(parameterize ((current-namespace (make-base-namespace))) ;Needed to avoid the otherwise empty namespace when calling load&create from top-level in game.rkt
    
    (let* ((mapfile (dynamic-require filename 'mapfile))
           (triggers (dynamic-require filename 'triggers))
           (tilemap (map-load mapfile triggers)))
      (new Map% 
           (sizex (vector-length (vector-ref tilemap 0)))
           (sizey (vector-length tilemap))
           (tiles tilemap))))

;(send map Load&Create 'torsk "testAI.txt")
