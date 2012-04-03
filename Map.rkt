#lang racket

(provide Map% load&create-map)

(require "Tile.rkt" "trigger.rkt" "Character.rkt")

(define Map%
  (class object%
    (super-new)
    (init-field sizex
                sizey
                tiles
                chars)
    
    (define/public (gettile gridx gridy) 
      (vector-ref (vector-ref tiles gridy) gridx))
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
    (define/public (get-tile-vector)
      tiles)))


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

(define (character-load char-list the-world)
  (let  ((xpos-in 0)
         (ypos-in 0)
         (gridx-in 0)
         (gridy-in 0)
         (triggerlist-in '())
         (AI-in (lambda () "I'm stupid"))
         (datafile #f))
    (define (load-from-file datafile)
      (define (readloop)
        (let ((data (read datafile)))
          (unless (eof-object? (peek-byte datafile))
            (case (car data)
              ((X) (set! xpos-in (* (cadr data) 32)))
              ((Y) (set! ypos-in (* (cadr data) 32)))
              ((GX) (set! gridx-in (cadr data)))
              ((GY) (set! gridy-in (cadr data)))
              ((triggerlist) (set! triggerlist-in (cadr data)))
              ((AI) (set! AI-in (cadr data))))
          (readloop))))
      (readloop)
      (close-input-port datafile)
      (new Character% 
           (xpos xpos-in)
           (ypos ypos-in)
           (gridx gridx-in)
           (gridy gridy-in)
           (triggerlist triggerlist-in)
           (AI-update AI-in)
           (world the-world)))
    (if (null? char-list)
        '()
        (begin
          (set! datafile (open-input-file (cdr (assq 'configfile (car char-list)))))
          (mcons (load-from-file datafile) (character-load(cdr char-list) the-world))))))


(define (load&create-map mapname filename world)
  (let* ((mapfile (dynamic-require filename 'mapfile))
         (triggers (dynamic-require filename 'triggers))
         (characters (character-load (dynamic-require filename 'characters) world))
         (tilemap (map-load mapfile triggers))
         (map-candidate (new Map% (sizex (vector-length (vector-ref tilemap 0)))
                             (sizey (vector-length tilemap))
                             (tiles tilemap)
                             (chars characters))))
    map-candidate))


