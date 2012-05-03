#lang racket

(require "Agent.rkt" "Trigger.rkt")

(provide Character%)
;--------------------------------------------------------------------------------------------
;Class: Agent
;Desc: this is a character, it has the properties of an agent, but it also has the ability to move,
;interact with stuff (including player) and a personality that is unique to every character
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
     interaction
     state)
    ;-----------------------
    ;this code is called when we want the character to move, the AI is individual.
    (init-field AI-update)
    ;_________________________
    
    ;------------------------------------------------------------------------------------------
    ;we remake the triggerlist to a list of triggers instead of a list of information.
    ;params: lst - the list of list of information that is defined in every characters infofile.
    (letrec ((loop (lambda (lst)
                     (if (null? lst)
                         '()
                         (cons (new Trigger% (trigger-assoc (car lst)))
                               (loop (cdr lst)))))))
      (set! triggerlist (loop triggerlist)))
    ;___________________________________________________________________________________________
    
    ;------------------------
    ;calls the function interaction that is defined in the characters infofile.
    (define/public (interact)
      (interaction))
    ;________________________
    
    
    ;--------------------------------
    ;some variables needed for the AI to work properly
    (define last-moved (box 0))
    (define last-stepped-on (box 0))
    (define chasing (box #f))
    ;________________________________
    
    ;------------------------
    ;simply returns true if the character is chasing the player.
    (define/public (chasing?)
      (unbox chasing))
    ;________________________
    
    ;--------------------------------------------------------------------------------------
    ;check wether the character is supposed to do something this tick, if so he/she does it.
    ;params: player-x/-y - the position of the player in the room.
    ;        ticks - the number of ticks since start.
    ;        world - the world in wich we are playing in.
    (define/public (update! player-x player-y ticks world)
      (for-each (lambda (trigger)
                  (send trigger poll&act this world))
                triggerlist)
      (AI-update this player-x player-y ticks last-moved last-stepped-on world chasing))
    ;______________________________________________________________________________________
    
    
    ;---------------------------------------------------------------------------------------
    ;move the character in the direction.
    ;params: direction - the direction wich the character has decided to move towards.
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
    ;________________________________________________________________________________________
    
    ;---------------------------------------------------------------------------------------
    ;sets moves the character to the position (x,y)
    ;params: x - new x
    ;        y - new y
    (define/public (set-pos! x y)
      (set! gridx x)
      (set! gridy y)
      (set! xpos (* 32 x))
      (set! ypos (* 32 y)))))
;___________________________________________________________________________________________
;_________________________________________________________________________________________________________
