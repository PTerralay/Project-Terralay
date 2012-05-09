#lang racket

(provide X Y GX GY triggers AI interact-code ID placement state type tex-ID)


(define state 1)
(define type 'monster)
(define GX 10)
(define GY 4)
(define X (* 32 GX))
(define Y (* 32 GY))
(define ID 'Eiresmile)
(define tex-ID 2)
(define placement 'Engineroom1)

(define triggers (list
                  ))

(define (interact-code)
  (display "Nom!"))

(define (AI monster player-x player-y ticks last-moved last-stepped-on world chasing)
  (when (> ticks (+ (unbox last-moved) 20))
    (let ((directionlist '())
          (distance-to-player-sqrd (+ (* (- player-x (get-field gridx monster))
                                         (- player-x (get-field gridx monster))) 
                                      (* (- player-y (get-field gridy monster))
                                         (- player-y (get-field gridy monster)))))
          (left-tile (send (get-field current-map world) gettile (- (get-field gridx monster) 1) (get-field gridy monster)))
          (right-tile (send (get-field current-map world) gettile (+ (get-field gridx monster) 1) (get-field gridy monster)))
          (up-tile (send (get-field current-map world) gettile (get-field gridx monster) (- (get-field gridy monster) 1)))
          (down-tile (send (get-field current-map world) gettile (get-field gridx monster) (+ (get-field gridy monster) 1)))
          (distance-up-sqrd (+ (* (- player-x (get-field gridx monster))
                                  (- player-x (get-field gridx monster)))
                               (* (- player-y (- (get-field gridy monster) 1))
                                  (- player-y (- (get-field gridy monster) 1)))))
          (distance-down-sqrd (+ (* (- player-x (get-field gridx monster))
                                    (- player-x (get-field gridx monster)))
                                 (* (- player-y (+ (get-field gridy monster) 1))
                                    (- player-y (+ (get-field gridy monster) 1)))))
          (distance-left-sqrd (+ (* (- player-x (- (get-field gridx monster) 1))
                                    (- player-x (- (get-field gridx monster) 1)))
                                 (* (- player-y (get-field gridy monster))
                                    (- player-y (get-field gridy monster)))))
          (distance-right-sqrd (+ (* (- player-x (+ (get-field gridx monster) 1))
                                     (- player-x (+ (get-field gridx monster) 1)))
                                  (* (- player-y (get-field gridy monster))
                                     (- player-y (get-field gridy monster))))))
      
      (if (< distance-to-player-sqrd 100)
          (begin
            ;let us know that we are chasing player
            (set-box! chasing #t)
            
            ;-----check if left tile is closer to player and passable------
            (when (< distance-left-sqrd
                     distance-to-player-sqrd)
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
            
            ;-----check if right tile is closer to player and passable-------
            (when (< distance-right-sqrd
                     distance-to-player-sqrd)
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
            
            ;-----check if "up" tile is closer to player and passable-------
            (when (< distance-up-sqrd
                     distance-to-player-sqrd)
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
            
            ;-----check if "down" tile is closer to player and passable-------
            (when (< distance-down-sqrd
                     distance-to-player-sqrd)
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
          
          (if (and (= 0 (random 99))
                     (not (eqv? (get-field mapID (get-field current-map world)) 'Engineroom1)))
                (set-box! chasing #f)
                (set! directionlist (cons (cons 0 'stay) directionlist))
                ))
      
      ;this is the actual movement-call, if the character has decided to move, then she will move in that direction.
      ;note that the monster will never back-track,
      ;meaning that the monster will not step on the tile that it last moved from. Hence the last-stepped-on variable.
      (if (not (null? directionlist))
          (begin (set-box! last-stepped-on
                           (send (get-field current-map world) gettile (get-field gridx monster) (get-field gridy monster)))
                 (send monster move! (cdr (argmin car directionlist)))
                 (set-box! last-moved ticks))
          (begin (set-box! last-stepped-on
                           (send (get-field current-map world) gettile (get-field gridx monster) (get-field gridy monster)))
                 (send monster move! 'stay)
                 (set-box! last-moved ticks))))))