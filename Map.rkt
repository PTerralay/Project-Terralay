#lang racket

(provide Map% load&create-map)

(require "Tile.rkt" "Trigger.rkt" "Character.rkt" "Thing.rkt"
         racket/mpair)

(define Map%
  (class object%
    (super-new)
    (init-field sizex
                sizey
                tiles
                chars
                things
                mapID
                neighbours)
    
    (define/public (gettile gridx gridy) 
      (vector-ref (vector-ref tiles gridy) gridx))
    (define/public (get-tile-vector)
      tiles)
    
    
    (define/public (render) 
      "not implemented yet!")
    
    (define/public (get-sizex)
      sizex)
    
    (define/public (get-sizey)
      sizey)
    
    (define/public (add-char! character)
      (set! chars (mcons character chars)))
    
    (define/public (get-characters)
      chars)
    
    (define/public (get-name)
      mapID)
    
    (define/public (get-neighbours)
      neighbours)
    
    (define/public (get-agents)
      (mappend (get-things) (get-characters)))
    
    (define/public (delete-character! char-id chars)
      (define (delete-helper charlist result)
        (cond ((null? charlist) result)
              ((eq? (send (mcar charlist) getname) char-id)
               (delete-helper (mcdr charlist) result))
              (else
               (begin
                 (set! result (mcons (mcar charlist) result))
                 (delete-helper (mcdr charlist) result)))))
      (delete-helper chars '()))
    
    
    (define/public (get-things)
      things)
    
    (define/public (add-thing! thing)
      (set! things (mcons thing things)))
    
    (define/public (delete-thing! thing)
      (define (delete-iter list)
        (cond ((null? list) (error "Place is empty"))
              ((eq? (mcar list) thing)
               (set! list (mcdr list))
               list)
              ((null? (mcdr list)) (error "Thing not found"))
              ((eq? (mcar (mcdr list)) thing)
               (set-mcdr! list (mcdr (mcdr list)))
               list)
              (else (mcons (mcar list) (delete-iter (mcdr list))))))
      (set! things (delete-iter things)))))

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


(define (load&create-map mapname filename world)
  (let* ((mapfile (dynamic-require filename 'mapfile))
         (triggers (dynamic-require filename 'triggers))
         ;----------------------------------------------------
         ;still bugged, find what's wrong until next time.
         (characters '())
         ;----------------------------------------------------
         (tilemap (map-load mapfile triggers))
         (stuff (Load-things (dynamic-require filename 'things-here) world))
         (neighbourlist (dynamic-require filename 'neighbours))
         (map-candidate (new Map% (sizex (vector-length (vector-ref tilemap 0)))
                             (sizey (vector-length tilemap))
                             (tiles tilemap)
                             (chars characters)
                             (things stuff)
                             (neighbours neighbourlist)
                             (mapID mapname))))
    (display "här\n")
    map-candidate))