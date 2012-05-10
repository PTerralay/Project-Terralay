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
    (init-field act-cond)
    
    (field 
     (animation-state 0)
     (gait-state #t)
     (in-transit #f)
     
     (speed 8)
     (dir 'up)
     (moved-last-tick #f)
     
     (targetx xpos)
     (targety ypos)
     (last-moved (box 0))
     (last-stepped-on (box 0))
     (chasing (box #f)))
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
      (AI this player-x player-y ticks last-moved last-stepped-on world chasing)
      (if moved-last-tick
          (begin
            (case dir
              ((up down)
               (when (eq? (remainder ypos 16) 0)
                 (if gait-state
                     (case animation-state
                       ((4) (set! animation-state 5))
                       ((5) (set! animation-state 6))
                       ((6) (set! animation-state 7)
                            (set! gait-state #f))
                       
                       ((12) (set! animation-state 13))
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
               (when (eq? (remainder xpos 16) 0)
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
                       ((18) (set! animation-state 17))))))))
          
          (set! moved-last-tick #t)))
    ;______________________________________________________________________________________
    
    
    ;---------------------------------------------------------------------------------------
    ;move the character in the direction.
    ;params: direction - the direction wich the character has decided to move towards.
    (define/public (move! direction) 
      (case direction
        ((up) (if (get-field passable (send (get-field current-map world) gettile gridx (- gridy 1)))
                  (if (< ypos targety)
                      (begin
                        (set! gridy (- gridy 1))
                        (set! in-transit #f))
                      (set! ypos (- ypos (/ 32 speed))))
                  (begin (set! in-transit #f)
                         ;                         (when (eq? (remainder ticks 20) 0)
                         ;                           (play (rs-read "Sounds/samples/kick_01_mono.wav"))
                         ;                           )
                         ))) 
        ((down) (if (get-field passable (send (get-field current-map world) gettile gridx (+ gridy 1))) 
                    (if (> ypos targety)
                        (begin
                          (set! gridy (+ gridy 1))
                          (set! in-transit #f))
                        (set! ypos (+ ypos (/ 32 speed))))
                    (begin (set! in-transit #f)
                           ;                           (when (eq? (remainder ticks 20) 0)
                           ;                             (play (rs-read "Sounds/samples/kick_01_mono.wav"))
                           ;                             )
                           ))) 
        ((left) (if (get-field passable (send (get-field current-map world) gettile (- gridx 1) gridy))
                    (if (< xpos targetx)
                        (begin
                          (set! gridx (- gridx 1))
                          (set! in-transit #f))
                        (set! xpos (- xpos (/ 32  speed))))
                    (begin (set! in-transit #f)
                           ;                           (when (eq? (remainder ticks 20) 0)
                           ;                             (play (rs-read "Sounds/samples/kick_01_mono.wav"))
                           ;                             )
                           )))
        ((right) (if (get-field passable (send (get-field current-map world) gettile (+ gridx 1) gridy)) 
                     (if (> xpos targetx)
                         (begin
                           (set! gridx (+ gridx 1))
                           (set! in-transit #f))
                         (set! xpos (+ xpos (/ 32 speed))))
                     (begin (set! in-transit #f)
                            ;                            (when (eq? (remainder ticks 20) 0)
                            ;                              (play (rs-read "Sounds/samples/kick_01_mono.wav"))
                            ;                              )
                            ))))
      
      
      
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
                   (set! xpos (+ xpos 32)))))
      )
    ;________________________________________________________________________________________
    
    ;---------------------------------------------------------------------------------------
    ;sets moves the character to the position (x,y)
    ;params: x - new x
    ;        y - new y
    
    (define/public (set-pos! x y)
      (set! gridx x)
      (set! gridy y)
      (set! xpos (* 32 x))
      (set! ypos (* 32 y)))
    
    
    ;--------------------------------
    ; movement-deciding
    ;--------------------------------
    
    (define (AI monster target-x target-y ticks last-moved last-stepped-on world chasing)
      (when (> ticks (+ (unbox last-moved) 20))
        (let ((directionlist '())
              (distance-to-target-sqrd (+ (* (- target-x (get-field gridx monster))
                                             (- target-x (get-field gridx monster))) 
                                          (* (- target-y (get-field gridy monster))
                                             (- target-y (get-field gridy monster)))))
              (left-tile (send (get-field current-map world) gettile (- (get-field gridx monster) 1) (get-field gridy monster)))
              (right-tile (send (get-field current-map world) gettile (+ (get-field gridx monster) 1) (get-field gridy monster)))
              (up-tile (send (get-field current-map world) gettile (get-field gridx monster) (- (get-field gridy monster) 1)))
              (down-tile (send (get-field current-map world) gettile (get-field gridx monster) (+ (get-field gridy monster) 1)))
              (distance-up-sqrd (+ (* (- target-x (get-field gridx monster))
                                      (- target-x (get-field gridx monster)))
                                   (* (- target-y (- (get-field gridy monster) 1))
                                      (- target-y (- (get-field gridy monster) 1)))))
              (distance-down-sqrd (+ (* (- target-x (get-field gridx monster))
                                        (- target-x (get-field gridx monster)))
                                     (* (- target-y (+ (get-field gridy monster) 1))
                                        (- target-y (+ (get-field gridy monster) 1)))))
              (distance-left-sqrd (+ (* (- target-x (- (get-field gridx monster) 1))
                                        (- target-x (- (get-field gridx monster) 1)))
                                     (* (- target-y (get-field gridy monster))
                                        (- target-y (get-field gridy monster)))))
              (distance-right-sqrd (+ (* (- target-x (+ (get-field gridx monster) 1))
                                         (- target-x (+ (get-field gridx monster) 1)))
                                      (* (- target-y (get-field gridy monster))
                                         (- target-y (get-field gridy monster))))))
          
          (if (act-cond world #t this)
              (begin
                ;let us know that we are chasing target
                (set-box! chasing #f)
                
                ;-----check if left tile is closer to target and passable------
                (when (< distance-left-sqrd
                         distance-to-target-sqrd)
                  (if (and (get-field passable left-tile)
                           (not (eq? (unbox last-stepped-on) left-tile)))
                      (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist))
                      (cond
                        ((and (even? (quotient ticks 20))
                              (get-field passable up-tile)
                              (not (eq? (unbox last-stepped-on) up-tile)))
                         (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist)))
                        ((and (get-field passable down-tile)
                              (not (eq? (unbox last-stepped-on) down-tile)))
                         (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist)))
                        ((and (get-field passable up-tile)
                              (not (eq? (unbox last-stepped-on) up-tile)))
                         (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))))))
                
                ;-----check if right tile is closer to target and passable-------
                (when (< distance-right-sqrd
                         distance-to-target-sqrd)
                  (if (and (get-field passable right-tile)
                           (not (eq? (unbox last-stepped-on) right-tile)))
                      (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist))
                      (cond
                        ((and (even? (quotient ticks 20))
                              (get-field passable up-tile)
                              (not (eq? (unbox last-stepped-on) up-tile)))
                         (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist)))
                        ((and (get-field passable down-tile)
                              (not (eq? (unbox last-stepped-on) down-tile)))
                         (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist)))
                        ((and (get-field passable up-tile)
                              (not (eq? (unbox last-stepped-on) up-tile)))
                         (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))))))
                
                ;-----check if "up" tile is closer to target and passable-------
                (when (< distance-up-sqrd
                         distance-to-target-sqrd)
                  (if (and (get-field passable up-tile)
                           (not (eq? (unbox last-stepped-on) up-tile)))
                      (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))
                      (cond
                        ((and (even? (quotient ticks 20))
                              (get-field passable left-tile)
                              (not (eq? (unbox last-stepped-on) left-tile)))
                         (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))
                        ((and (get-field passable right-tile)
                              (not (eq? (unbox last-stepped-on) right-tile)))
                         (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist)))
                        ((and (get-field passable left-tile)
                              (not (eq? (unbox last-stepped-on) left-tile)))
                         (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist))))))
                
                ;-----check if "down" tile is closer to target and passable-------
                (when (< distance-down-sqrd
                         distance-to-target-sqrd)
                  (if (and (get-field passable down-tile)
                           (not (eq? (unbox last-stepped-on) down-tile)))
                      (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist))
                      (cond
                        ((and (even? (quotient ticks 20))
                              (get-field passable left-tile)
                              (not (eq? (unbox last-stepped-on) left-tile)))
                         (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))
                        ((and (get-field passable right-tile)
                              (not (eq? (unbox last-stepped-on) right-tile)))
                         (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist)))
                        ((and (get-field passable left-tile)
                              (not (eq? (unbox last-stepped-on) left-tile)))
                         (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))))))
              
              (set! directionlist (cons (cons 0 '(act-cond world #t this)) directionlist)))
          
          ;this is the actual movement-call, if the character has decided to move, then she will move in that direction.
          ;note that the monster will never back-track,
          ;meaning that the monster will not step on the tile that it last moved from. Hence the last-stepped-on variable.
          (if (not (null? directionlist))
              (begin (set-box! last-stepped-on
                               (send (get-field current-map world) gettile (get-field gridx monster) (get-field gridy monster)))
                     (set! dir (argmin car directionlist))
                     (set-box! last-moved ticks)
                     (case dir
                       ((up) (set! animation-state 4)
                             (set! targety (* (- gridy 1) 32)))
                       ((right) (set! animation-state 8)
                                (set! targetx (* (+ gridx 1) 32)))
                       ((down) (set! animation-state 12)
                               (set! targety (* (+ gridy 1) 32)))
                       ((left) (set! animation-state 16)
                               (set! targetx (* (- gridx 1) 32))))
                     (set! moved-last-tick #t)
                     (move! (cdr (argmin car directionlist))))
              (begin (set-box! last-stepped-on
                               (send (get-field current-map world) gettile (get-field gridx monster) (get-field gridy monster)))
                     
                     (set-box! last-moved ticks))))))))
;___________________________________________________________________________________________
;_________________________________________________________________________________________________________
