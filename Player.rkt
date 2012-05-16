#lang racket/gui

(require racket/mpair
         racket/gui
         (planet "main.rkt" ("clements" "rsound.plt" 3 2)))

(provide Player%)
;-----------------------------------------------------------------------------------
;Class: object
;Desc: this is the players representation in the world.
;the player is controlable via the keyboard and is able to interact with the world.
;-----------------------------------------------------------------------------------
(define Player%
  (class object%
    (super-new)
    
    (init-field xpos
                ypos
                gridx
                gridy
                dir
                world
                glcanvas
                speed
                inventory)
    ;-----------------------------------------------------------------------------------
    ;the direction in wich the lantern 
    ;-----------------------------------------------------------------------------------
    (field (angle
            (case dir
              ((left) 270)
              ((up) 0)
              ((right) 90)
              ((down) 180)))
           (gait-state #t)
           (animation-state (case dir
                              ((left) 0)
                              ((up) 1)
                              ((right) 2)
                              ((down) 3)))
           
           (transitstate 0)
           (targetx xpos)
           (targety ypos)
           (in-transit #f)
           (moved-last-tick #f))
    
    ;-----------------------------------------------------------------------------------
    ;sets the direction of the player to new-dir
    ;-----------------------------------------------------------------------------------
    (define/public (set-dir! new-dir)
      (set! dir new-dir))
    
    ;-----------------------------------------------------------------
    ;check if there is an agent where we're trying to interact/move
    ;------------------------------------------------------------------
    (define (agent? x y)
      (findf (lambda (agent)
               (and (eqv? (get-field place agent)
                          (get-field mapID (get-field current-map world)))
                    (eqv? (get-field gridx agent) x)
                    (eqv? (get-field gridy agent) y)))
             (mlist->list (get-field agents world))))
    
    ;-----------------------------------------------------------------------------------
    ;if there is an agent in the direction we are facing player will interact with it.
    ;-----------------------------------------------------------------------------------
    
    (define/public (interact with)
      (display "interacting")
      
      ;checks wich direction we are facing and interacts with eventual agent.
      
      (case dir
        ((left) (display " left")
                (when (agent? (- gridx 1) gridy)
                  (display " with ")
                  (display (agent? (- gridx 1) gridy))
                  (send (agent? (- gridx 1) gridy) interact with)))
        ((right) (display " right")
                 (when (agent? (+ gridx 1) gridy)
                   (display " with ")
                   (display (agent? (+ gridx 1) gridy))
                   (send (agent? (+ gridx 1) gridy) interact with)))
        ((up) (display " up")
              (when (agent? gridx (- gridy 1))
                (display " with ")
                (display (agent? gridx (- gridy 1)))
                (send (agent? gridx (- gridy 1)) interact with)))
        ((down) (display " down")
                (when (agent? gridx (+ gridy 1))
                  (display " with ")
                  (display (agent? gridx (+ gridy 1)))
                  (send (agent? gridx (+ gridy 1)) interact with)))))
    
    ;-----------------------------------------------------------------------------------
    ;moves the player in direction and animates his movement during transit.
    ;params: direction - the direction player is trying to move.
    ;-----------------------------------------------------------------------------------
    (define/public (move! direction ticks)
      (let ((agentup (agent? gridx (- gridy 1)))
            (agentdown (agent? gridx (+ gridy 1)))
            (agentleft (agent? (- gridx 1) gridy))
            (agentright (agent? (+ gridx 1) gridy)))
        
        (case direction
          ((up) (if (and (get-field passable (send (get-field current-map world) gettile gridx (- gridy 1)))
                         (if (object? agentup)
                             (if (eqv? (get-field mapID (get-field current-map world))
                                            (get-field place agentup))
                                 (get-field passable agentup)
                                 #t)
                             #t))
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
          ((down) (if (and (get-field passable (send (get-field current-map world) gettile gridx (+ gridy 1)))
                           (if (object? agentdown)
                               (if (eqv? (get-field mapID (get-field current-map world))
                                              (get-field place agentdown))
                                   (get-field passable agentdown)
                                   #t)
                               #t))
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
          ((left) (if (and (get-field passable (send (get-field current-map world) gettile (- gridx 1) gridy))
                           (if (object? agentleft)
                               (if (eqv? (get-field mapID (get-field current-map world))
                                              (get-field place agentleft))
                                   (get-field passable agentleft)
                                   #t)
                               #t))
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
          ((right) (if (and (get-field passable (send (get-field current-map world) gettile (+ gridx 1) gridy))
                            (if (object? agentright)
                                (if (eqv? (get-field mapID (get-field current-map world))
                                               (get-field place agentright))
                                    (get-field passable agentright)
                                    #t)
                                #t))
                       (if (> xpos targetx)
                           (begin
                             (set! gridx (+ gridx 1))
                             (set! in-transit #f))
                           (set! xpos (+ xpos (/ 32 speed))))
                       (begin (set! in-transit #f)
                              ;                            (when (eq? (remainder ticks 20) 0)
                              ;                              (play (rs-read "Sounds/samples/kick_01_mono.wav"))
                              ;                              )
                              ))))))
    
    
    ;----------------- Only for movement between maps or triggers ----------------------
    ;will set the players position to be (x,y)
    ;-----------------------------------------------------------------------------------
    (define/public (set-pos! x y)
      (set! xpos (* 32 x))
      (set! ypos (* 32 y))
      (set! gridx x)
      (set! gridy y))
    
    ;-----------------------------------------------------------------------------------
    ;checks what player is trying to do, wether it is moving, interacting or turning
    ;-----------------------------------------------------------------------------------
    (define/public (update! ticks)
      
      ;------- Movement ---------
      (let ((keys (get-field keys glcanvas))
            (last-key (get-field last-key glcanvas))
            (left 0)
            (right 1)
            (up 2)
            (down 3)
            (space 4)
            (facing dir))
        (unless in-transit
          (if moved-last-tick
              (set! moved-last-tick #f)
              (set! animation-state (case dir
                                      ((up) 0)
                                      ((right) 1)
                                      ((down) 2)
                                      ((left) 3))))
          (cond ((and (vector-ref keys left) 
                      (not (vector-ref keys right))
                      (not (vector-ref keys up))
                      (not (vector-ref keys down)))
                 (set! dir 'left)
                 (set! targetx (- (* gridx 32) 32)))
                
                ((and (not (vector-ref keys left))
                      (vector-ref keys right)
                      (not (vector-ref keys up))
                      (not (vector-ref keys down)))
                 (set! dir 'right)
                 (set! targetx (+ (* gridx 32) 32)))
                
                ((and (not (vector-ref keys left))
                      (not (vector-ref keys right))
                      (vector-ref keys up)
                      (not (vector-ref keys down)))
                 (set! dir 'up)
                 (set! targety (- (* gridy 32) 32)))
                
                ((and (not (vector-ref keys left))
                      (not (vector-ref keys right))
                      (not (vector-ref keys up))
                      (vector-ref keys down))
                 (set! dir 'down)
                 (set! targety (+ (* gridy 32) 32))))
          
          (case last-key
            ((left) (when (vector-ref keys left)
                      (set! dir 'left)
                      (set! targetx (- (* gridx 32) 32))))
            ((right) (when (vector-ref keys right)
                       (set! dir 'right)
                       (set! targetx (+ (* gridx 32) 32))))
            ((up) (when (vector-ref keys up)
                    (set! dir 'up)
                    (set! targety (- (* gridy 32) 32))))
            ((down) (when (vector-ref keys down)
                      (set! dir 'down)
                      (set! targety (+ (* gridy 32) 32)))))
          
          
          (when (and (or (vector-ref keys left) 
                         (vector-ref keys right) 
                         (vector-ref keys up) 
                         (vector-ref keys down))
                     (eq? dir facing))
            (set! in-transit #t)
            (set! gait-state #t)
            (case dir
              ((up) (set! animation-state 4))
              ((right) (set! animation-state 8))
              ((down) (set! animation-state 12))
              ((left) (set! animation-state 16)))))
        (when in-transit
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
              
              (set! moved-last-tick #t))
          (move! dir ticks)))
      
      ;-------- Rotate the fov gradually -----
      
      (case dir
        ((up)
         (if (and (< angle 360) (> angle 179))
             (if (= angle 315)
                 (set! angle 0)
                 (set! angle (+ angle 45)))
             (unless (= angle 0)
               (set! angle (- angle 45)))))
        ((down)
         (if (and (> angle -1) (< angle 180))
             (set! angle (+ angle 45))
             (unless (= angle 180)
               (set! angle (- angle 45)))))
        ((left)
         (if (and (> angle 89) (< angle 270))
             (set! angle (+ angle 45))
             (unless (= angle 270)
               (if (= angle 0)
                   (set! angle 315)
                   (set! angle (- angle 45))))))
        ((right)
         (if (and (< angle 270) (> angle 90))
             (set! angle (- angle 45))
             (unless (= angle 90)
               (if (= angle 315)
                   (set! angle 0)
                   (set! angle (+ angle 45))))))))))

