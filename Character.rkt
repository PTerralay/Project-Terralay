#lang racket

(require "Agent.rkt" "Trigger.rkt")

(provide Character%)

(define Character%
  (class Agent%
    (super-new)
    (inherit-field 
     xpos
     ypos
     gridx
     gridy
     triggerlist
     world
     agent-ID)
    (init-field AI-update
                interaction)
        (letrec ((loop (lambda (lst)
                     (if (null? lst)
                         '()
                         (cons (new Trigger% (trigger-assoc (car lst)))
                               (loop (cdr lst)))))))
      (set! triggerlist (loop triggerlist)))
    
    
    (define/public (interact)
      (interaction))
    
    (define last-moved (box 0))
    (define last-stepped-on (box 0))
    (define chasing (box #f))
    
    (define/public (chasing?)
      (unbox chasing))

    (define/public (update! player-x player-y ticks world)
      (for-each (lambda (trigger)
                  (send trigger poll&act this world))
                triggerlist)
      (AI-update this player-x player-y ticks last-moved last-stepped-on world chasing))
    
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
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    
    (define/public (set-pos x y)
      (set! gridx x)
      (set! gridy y)
      (set! xpos (* 32 x))
      (set! ypos (* 32 y)))))



;-----------------------------------------------------------;

;(define testmap (Load&Create 'testmap "Loadtest.txt"))
;(define Trollworld (new World% (maplist '(testmap)) (current-map testmap) (state 1)))


; (set! last-moved ticks)))))
