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
     place
     agent-ID
     interaction)
    (init-field AI-update)
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
        ((up) (when (get-field passable (send (get-field current-map world) gettile gridx (- gridy 1))) 
                (set! gridy (- gridy 1))
                (set! ypos (- ypos 32)))) 
        ((down) (when (get-field passable (send (get-field current-map world) gettile gridx (+ gridy 1))) 
                  (set! gridy (+ gridy 1))
                  (set! ypos (+ ypos 32))))
        ((left) (when (get-field passable (send (get-field current-map world) gettile (- gridx 1) gridy)) 
                  (set! gridx (- gridx 1))
                  (set! xpos (- xpos 32))))
        ((right) (when (get-field passable (send (get-field current-map world) gettile (+ gridx 1) gridy)) 
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
