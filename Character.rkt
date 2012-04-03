#lang racket

(require "Agent.rkt")

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
    (define last-moved (box 0))
    (define/public (update! pl-x pl-y ticks)
      ((load AI-update) this pl-x pl-y ticks last-moved))
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
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)))



;-----------------------------------------------------------;

;(define testmap (Load&Create 'testmap "Loadtest.txt"))
;(define Trollworld (new World% (maplist '(testmap)) (current-map testmap) (state 1)))


; (set! last-moved ticks)))))
