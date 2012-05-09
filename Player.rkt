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
           (transitstate 0)
           (targetx xpos)
           (targety ypos)
           (in-transit #f))
    
    ;-----------------------------------------------------------------------------------
    ;sets the direction of the player to new-dir
    ;-----------------------------------------------------------------------------------
    (define/public (set-dir! new-dir)
      (set! dir new-dir))
    
    
    ;-----------------------------------------------------------------------------------
    ;if there is an agent in the direction we are facing player will interact with it.
    ;-----------------------------------------------------------------------------------
    
    (define/public (interact along-with)
      (display "interacting")
      
      ;check if there is an agent where we're trying to interact
      (define (agent? x y)
        (findf (lambda (agent)
                 (and (eqv? (get-field place agent)
                            (get-field mapID (get-field current-map world)))
                      (eqv? (get-field gridx agent) x)
                      (eqv? (get-field gridy agent) y)))
               (mlist->list (get-field agents world))))
      ;checks wich direction we are facing and interacts with eventual agent.
      (case dir
        ((left) (display " left")
                (when (agent? (- gridx 1) gridy)
                  (display " with ")
                  (display (agent? (- gridx 1) gridy))
                  (send (agent? (- gridx 1) gridy) interact along-with)))
        ((right) (display " right")
                 (when (agent? (+ gridx 1) gridy)
                   (display " with ")
                   (display (agent? (+ gridx 1) gridy))
                   (send (agent? (+ gridx 1) gridy) interact along-with)))
        ((up) (display " up")
              (when (agent? gridx (- gridy 1))
                (display " with ")
                (display (agent? gridx (- gridy 1)))
                (send (agent? gridx (- gridy 1)) interact along-with)))
        ((down) (display " down")
                (when (agent? gridx (+ gridy 1))
                  (display " with ")
                  (display (agent? gridx (+ gridy 1)))
                  (send (agent? gridx (+ gridy 1)) interact along-with)))))
    
    ;-----------------------------------------------------------------------------------
    ;moves the player in direction and animates his movement during transit.
    ;params: direction - the direction player is trying to move.
    ;-----------------------------------------------------------------------------------
    (define/public (move! direction ticks)
      (case direction
        ((up) (if (get-field passable (send (get-field current-map world) gettile gridx (- gridy 1)))
                  (if (< ypos targety)
                      (begin
                        (set! gridy (- gridy 1))
                        (set! in-transit #f))
                      (set! ypos (- ypos (/ 32 speed))))
                  (begin (set! in-transit #f)
                         (when (eq? (remainder ticks 20) 0)
                           (play (rs-read "Sounds/samples/kick_01_mono.wav")))))) 
        ((down) (if (get-field passable (send (get-field current-map world) gettile gridx (+ gridy 1))) 
                    (if (> ypos targety)
                        (begin
                          (set! gridy (+ gridy 1))
                          (set! in-transit #f))
                        (set! ypos (+ ypos (/ 32 speed))))
                    (begin (set! in-transit #f)
                           (when (eq? (remainder ticks 20) 0)
                             (play (rs-read "Sounds/samples/kick_01_mono.wav")))))) 
        ((left) (if (get-field passable (send (get-field current-map world) gettile (- gridx 1) gridy))
                    (if (< xpos targetx)
                        (begin
                          (set! gridx (- gridx 1))
                          (set! in-transit #f))
                        (set! xpos (- xpos (/ 32  speed))))
                    (begin (set! in-transit #f)
                           (when (eq? (remainder ticks 20) 0)
                             (play (rs-read "Sounds/samples/kick_01_mono.wav"))))))
        ((right) (if (get-field passable (send (get-field current-map world) gettile (+ gridx 1) gridy)) 
                     (if (> xpos targetx)
                         (begin
                           (set! gridx (+ gridx 1))
                           (set! in-transit #f))
                         (set! xpos (+ xpos (/ 32 speed))))
                     (begin (set! in-transit #f)
                            (when (eq? (remainder ticks 20) 0)
                              (play (rs-read "Sounds/samples/kick_01_mono.wav"))))))))
    
    (define/public (render) "not implemented yet")
    
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
        (when (not in-transit)
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
            (set! in-transit #t)))
        (when in-transit
          (move! dir ticks)))
      
      ;-------- Check the tile triggers ------
      (let ((tile (send (get-field current-map world) gettile gridx gridy)))
        (for-each (lambda (trigger)
                    (send trigger poll&act tile world))
                  (get-field triggerlist tile)))
      
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

