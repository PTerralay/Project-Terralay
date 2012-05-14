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
     tex-ID
     interaction
     state)
    
    ;-----------------------
    ;this code is called when we want the character to move, the AI is individual.
    (init-field move-cond)
    
    (field 
     (animation-state 0)
     (gait-state #t)
     (in-transit #f)
     
     (directionlist '())
     
     (speed 1)
     (dir 'up)
     (moved-last-tick #f)
     
     (targetx xpos)
     (targety ypos)
     (last-moved 0)
     (last-stepped-on 0)
     (chasing #f))
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
    (define/public (interact cmd)
      (interaction cmd))
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
      (display animation-state)
      (display " ") (display gait-state) (display " ") (display (remainder xpos 16))
      (newline)
      
      
      (if in-transit
          (begin
                (case dir 
                  ((up down)
                   (when (eq? (remainder ypos 4) 0)
                     (if gait-state
                         (case animation-state
                           ((4) (set! animation-state 5))
                           ((5) (set! animation-state 6))
                           ((6) (set! animation-state 7)
                                (set! gait-state #f))
                           
                           ((12) (display "IMGOINGDOWN")
                                 (set! animation-state 13))
                           ((13) (set! animation-state 14))
                           ((14) (set! animation-state 15)
                                 (set! gait-state #f)))
                         (case animation-state 
                           ((7) (set! animation-state 6))
                           ((5) (set! animation-state 4)
                                (set! gait-state #t))
                           ((6) (set! animation-state 5))
                           
                           ((15) (set! animation-state 14))
                           ((13) (set! animation-state 12)
                                 (set! gait-state #t))
                           ((14) (set! animation-state 13))))))
                  ((left right)
                   (when (eq? (remainder xpos 4) 0)
                     (if gait-state
                         (case animation-state
                           ((8) (set! animation-state 9))
                           ((9) (set! animation-state 10))
                           ((10) (set! animation-state 11)
                                 (set! gait-state #f))
                           
                           ((16) (set! animation-state 17))
                           ((17) (set! animation-state 18))
                           ((18) (set! animation-state 19)
                                 (set! gait-state #f)))
                         (case animation-state 
                           ((11) (set! animation-state 10))
                           ((9) (set! animation-state 8)
                                (set! gait-state #t))
                           ((10) (set! animation-state 9))
                           
                           ((19) (set! animation-state 18))
                           ((17) (set! animation-state 16)
                                 (set! gait-state #t))
                           ((18) (set! animation-state 17)))))))
            (when (not (null? directionlist))
              (move!)))
          (begin
            (set! animation-state (case dir
                                    ((up) 0)
                                    ((right) 1)
                                    ((down) 2)
                                    ((left) 3)))
            (AI player-x player-y ticks))))
    ;______________________________________________________________________________________
    
    
    ;---------------------------------------------------------------------------------------
    ;move the character in the direction.
    ;params: direction - the direction that the character has decided to move towards.
    (define/public (move!) 
      (case dir
        ((up) (if (get-field passable (send (get-field current-map world) gettile gridx (- gridy 1)))
                  (if (< ypos targety)
                      (begin
                        (set! gridy (- gridy 1))
                        (set! in-transit #f))
                      (set! ypos (- ypos speed)))
                  (begin (set! in-transit #f)))) 
        ((down) (if (get-field passable (send (get-field current-map world) gettile gridx (+ gridy 1))) 
                    (if (> ypos targety)
                        (begin
                          (set! gridy (+ gridy 1))
                          (set! in-transit #f))
                        (set! ypos (+ ypos speed)))
                    (begin (set! in-transit #f)))) 
        ((left) (if (get-field passable (send (get-field current-map world) gettile (- gridx 1) gridy))
                    (if (< xpos targetx)
                        (begin
                          (set! gridx (- gridx 1))
                          (set! in-transit #f))
                        (set! xpos (- xpos speed)))
                    (begin (set! in-transit #f))))
        ((right) (if (get-field passable (send (get-field current-map world) gettile (+ gridx 1) gridy)) 
                     (if (> xpos targetx)
                         (begin
                           (set! gridx (+ gridx 1))
                           (set! in-transit #f))
                         (set! xpos (+ xpos speed)))
                     (begin (set! in-transit #f))))))
    ;________________________________________________________________________________________
    
    ;---------------------------------------------------------------------------------------
    ;sets moves the character to the position (x,y)
    ;params: x - new x
    ;        y - new y
    ;---------------------------------------------------------------------------------------
    (define/public (set-pos! x y)
      (set! gridx x)
      (set! gridy y)
      
      (set! xpos (* 32 x))
      (set! ypos (* 32 y))
      (case dir
          ((up) (set! animation-state 4)
                (set! targety (* (- gridy 1) 32)))
          ((right) (set! animation-state 8)
                   (set! targetx (* (+ gridx 1) 32)))
          ((down) (set! animation-state 12)
                  (set! targety (* (+ gridy 1) 32)))
          ((left) (set! animation-state 16)
                  (set! targetx (* (- gridx 1) 32)))))
    
    
    ;--------------------------------
    ; movement-deciding
    ;--------------------------------
    (define/private (AI target-x target-y ticks)
      (let ((distance-to-target-sqrd (+ (sqr (- target-x gridx))
                                        (sqr (- target-y gridy))))
            (left-tile (send (get-field current-map world) gettile (- gridx 1) gridy))
            (right-tile (send (get-field current-map world) gettile (+ gridx 1) gridy))
            (up-tile (send (get-field current-map world) gettile gridx (- gridy 1)))
            (down-tile (send (get-field current-map world) gettile gridx (+ gridy 1)))
            (distance-up-sqrd (+ (sqr (- target-x gridx))
                                 (sqr (- target-y (- gridy 1)))))
            (distance-down-sqrd (+ (sqr (- target-x gridx))
                                   (sqr (- target-y (+ gridy 1)))))
            (distance-left-sqrd (+ (sqr (- target-x (- gridx 1)))
                                   (sqr (- target-y gridy))))
            (distance-right-sqrd (+ (sqr (- target-x (+ gridx 1)))
                                    (sqr (- target-y gridy))))
            (what-should-i-do? (move-cond world this)))
        
        
        (set! directionlist '())
        
        ; If what-should-i-do? returns 'move, then the move algorithm is run. If it's 'stay the monster
        ; won't do a thing, and if it's something else it is assumed to be a procedure and it is then applied.
        (cond ((eq? what-should-i-do? 'move)
               ;let us know that we are chasing target
               (set! chasing #t)
               
               (set! in-transit #t)
               (set! gait-state #t)
               (set! last-moved ticks)
               (case dir
                 ((up) (set! animation-state 4))
                 ((right) (set! animation-state 8))
                 ((down) (set! animation-state 12))
                 ((left) (set! animation-state 16)))
               ;-----check if left tile is closer to target and passable------
               (when (< distance-left-sqrd
                        distance-to-target-sqrd)
                 (if (and (get-field passable left-tile)
                          (not (eq? last-stepped-on left-tile)))
                     (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist))
                     (cond
                       ((and (even? ticks)
                             (get-field passable up-tile)
                             (not (eq? last-stepped-on up-tile)))
                        (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist)))
                       ((and (get-field passable down-tile)
                             (not (eq? last-stepped-on down-tile)))
                        (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist)))
                       ((and (get-field passable up-tile)
                             (not (eq? last-stepped-on up-tile)))
                        (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))))))
               
               ;-----check if right tile is closer to target and passable-------
               (when (< distance-right-sqrd
                        distance-to-target-sqrd)
                 (if (and (get-field passable right-tile)
                          (not (eq? last-stepped-on right-tile)))
                     (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist))
                     (cond
                       ((and (even? ticks)
                             (get-field passable up-tile)
                             (not (eq? last-stepped-on up-tile)))
                        (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist)))
                       ((and (get-field passable down-tile)
                             (not (eq? last-stepped-on down-tile)))
                        (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist)))
                       ((and (get-field passable up-tile)
                             (not (eq? last-stepped-on up-tile)))
                        (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))))))
               
               ;-----check if "up" tile is closer to target and passable-------
               (when (< distance-up-sqrd
                        distance-to-target-sqrd)
                 (if (and (get-field passable up-tile)
                          (not (eq? last-stepped-on up-tile)))
                     (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))
                     (cond
                       ((and (even? ticks)
                             (get-field passable left-tile)
                             (not (eq? last-stepped-on left-tile)))
                        (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))
                       ((and (get-field passable right-tile)
                             (not (eq? last-stepped-on right-tile)))
                        (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist)))
                       ((and (get-field passable left-tile)
                             (not (eq? last-stepped-on left-tile)))
                        (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist))))))
               
               
               ;-----check if "down" tile is closer to target and passable-------
               (when (< distance-down-sqrd
                        distance-to-target-sqrd)
                 (if (and (get-field passable down-tile)
                          (not (eq? last-stepped-on down-tile)))
                     (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist))
                     (cond
                       ((and (even? ticks)
                             (get-field passable left-tile)
                             (not (eq? last-stepped-on left-tile)))
                        (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))
                       ((and (get-field passable right-tile)
                             (not (eq? last-stepped-on right-tile)))
                        (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist)))
                       ((and (get-field passable left-tile)
                             (not (eq? last-stepped-on left-tile)))
                        (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))))))
              ((eq? what-should-i-do? 'stay)
               (void))
              (else
               (set! chasing #f)
               (what-should-i-do? world this)))
        
        ;this is the actual movement-call, if the character has decided to move, then she will move in that direction.
        ;note that the monster will never back-track,
        ;meaning that the monster will not step on the tile that it last moved from. Hence the last-stepped-on variable.
        (set! last-stepped-on
              (send (get-field current-map world) gettile gridx gridy))
        (set! last-moved ticks)
        (send (get-field current-map world) gettile gridx gridy)
        (unless (null? directionlist)
          (set! dir (cdr (argmin car directionlist))))
        (case dir
          ((up) (set! animation-state 4)
                (set! targety (* (- gridy 1) 32)))
          ((right) (set! animation-state 8)
                   (set! targetx (* (+ gridx 1) 32)))
          ((down) (set! animation-state 12)
                  (set! targety (* (+ gridy 1) 32)))
          ((left) (set! animation-state 16)
                  (set! targetx (* (- gridx 1) 32))))))))
