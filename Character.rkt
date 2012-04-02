#lang racket

(require "Agent.rkt" "world.rkt" "Map.rkt" "Tile.rkt")

(provide Character%)

(define Character%
  (class Agent%
    (super-new)
    (inherit-field xpos
                   ypos
                   gridx
                   gridy
                   triggerlist
                   world)
    (init-field AI-update)
    
    (define/public (AI-update!) ((eval AI-update)))
    (define/public (talk-to) "not implemented yet")
    
    (define/public (move! direction) 
      (case direction
        ((up) (when (send (send (send world get-current-map) gettile gridx (- gridy 1)) passable?) 
                (set! gridy (- gridy 1))
                (set! ypos (- ypos 32)))) 
        ((down) (when (send (send (send world get-current-map) gettile gridx (+ gridy 1)) passable?) 
                  (set! gridy (+ gridy 1))
                  (set! ypos (+ ypos 32))))
        ((left) (when (send (send (send world get-current-map) gettile (- gridx 1) gridy) passable?) 
                  (set! gridx (- gridx 1))
                  (set! xpos (- xpos 32))))
        ((right) (when (send (send (send world get-current-map) gettile (+ gridx 1) gridy) passable?) 
                   (set! gridx (+ gridx 1))
                   (set! xpos (+ xpos 32))))))
    
    (define/public (gety)
      gridy)
    (define/public (getx)
      gridx)
    ))

(define (LoadChar filename world-in)
  (let ((xpos-in 0)
        (ypos-in 0)
        (gridx-in 0)
        (gridy-in 0)
        (triggerlist-in '())
        (AI-in (lambda () "I'm stupid"))
        (datafile (open-input-file filename)))
    
    (define (readloop)
      (unless (eof-object? (peek-byte datafile))
        (let ((data (read datafile)))
          (case (car data)
            ((X) (set! xpos-in (* (cadr data) 32)))
            ((Y) (set! ypos-in (* (cadr data) 32)))
            ((GX) (set! gridx-in (cadr data)))
            ((GY) (set! gridy-in (cadr data)))
            ((triggerlist) (set! triggerlist-in (cadr data)))
            ((AI) (set! AI-in (cadr data)))))
        (readloop)))
    (readloop)
    (close-input-port datafile)
    (let ((character (new Character% 
                          (xpos xpos-in)
                          (ypos ypos-in)
                          (gridx gridx-in)
                          (gridy gridy-in)
                          (triggerlist triggerlist-in)
                          (world world-in)
                          (AI-update AI-in))))
      (send (send world-in get-current-map) Add-agent! character)
      character)))
;-----------------------------------------------------------;

(define testmap (Load&Create 'testmap "Loadtest.txt"))
(define Trollworld (new World% (maplist '(testmap)) (current-map testmap) (state 1)))