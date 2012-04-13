#lang racket

(provide X Y GX GY triggers AI interact-code ID)

(define X 320)
(define Y 128)
(define GX 10)
(define GY 4)
(define ID "Tetsy")
(define triggers (list
                  (list
                   (cons 'poll (lambda (char world)
                                 (and (eq? (send (send world get-player) getx) (send char getx))
                                      (eq? (send (send world get-player) gety) (send char gety)))))
                   (cons 'act (lambda (char world)
                                (display "GOTCHA YOU PRETTY PRETTY LITTLE GIRL! Now come here...\n"))))))

(define (interact-code)
  (display "Nom!"))

(define (AI monster player-x player-y ticks last-moved last-stepped-on world chasing)
  (when (> ticks (+ (unbox last-moved) 20))
    (let ((directionlist '())
          (direction-int (random 3))
          (distance-to-player-sqrd (+ (* (- player-x (send monster getx))
                                         (- player-x (send monster getx))) 
                                      (* (- player-y (send monster gety))
                                         (- player-y (send monster gety)))))
          (left-tile (send (send world get-current-map) gettile (- (send monster getx) 1) (send monster gety)))
          (right-tile (send (send world get-current-map) gettile (+ (send monster getx) 1) (send monster gety)))
          (up-tile (send (send world get-current-map) gettile (send monster getx) (- (send monster gety) 1)))
          (down-tile (send (send world get-current-map) gettile (send monster getx) (+ (send monster gety) 1)))
          (distance-up-sqrd (+ (* (- player-x (send monster getx))
                                  (- player-x (send monster getx)))
                               (* (- player-y (- (send monster gety) 1))
                                  (- player-y (- (send monster gety) 1)))))
          (distance-down-sqrd (+ (* (- player-x (send monster getx))
                                    (- player-x (send monster getx)))
                                 (* (- player-y (+ (send monster gety) 1))
                                    (- player-y (+ (send monster gety) 1)))))
          (distance-left-sqrd (+ (* (- player-x (- (send monster getx) 1))
                                    (- player-x (- (send monster getx) 1)))
                                 (* (- player-y (send monster gety))
                                    (- player-y (send monster gety)))))
          (distance-right-sqrd (+ (* (- player-x (+ (send monster getx) 1))
                                     (- player-x (+ (send monster getx) 1)))
                                  (* (- player-y (send monster gety))
                                     (- player-y (send monster gety))))))
      
      (if (< distance-to-player-sqrd 100)
          (begin
            (set-box! chasing #t) ;let us know that we are chasing player
            ;-----left------
            (when (< distance-left-sqrd
                     distance-to-player-sqrd)
              (if (and (send left-tile passable?)
                       (not (eq? (unbox last-stepped-on) left-tile)))
                  (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist))
                  (cond
                    ((and (even? (quotient ticks 20))
                          (send up-tile passable?)
                          (not (eq? (unbox last-stepped-on) up-tile)))
                     (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist)))
                    ((and (send down-tile passable?)
                          (not (eq? (unbox last-stepped-on) down-tile)))
                     (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist)))
                    ((and (send up-tile passable?)
                          (not (eq? (unbox last-stepped-on) up-tile)))
                     (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))))))
            ;-----right-------
            (when (< distance-right-sqrd
                     distance-to-player-sqrd)
              (if (and (send right-tile passable?)
                       (not (eq? (unbox last-stepped-on) right-tile)))
                  (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist))
                  (cond
                    ((and (even? (quotient ticks 20))
                          (send up-tile passable?)
                          (not (eq? (unbox last-stepped-on) up-tile)))
                     (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist)))
                    ((and (send down-tile passable?)
                          (not (eq? (unbox last-stepped-on) down-tile)))
                     (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist)))
                    ((and (send up-tile passable?)
                          (not (eq? (unbox last-stepped-on) up-tile)))
                     (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))))))
            ;------up---------
            (when (< distance-up-sqrd
                     distance-to-player-sqrd)
              (if (and (send up-tile passable?)
                       (not (eq? (unbox last-stepped-on) up-tile)))
                  (set! directionlist (cons (cons distance-up-sqrd 'up) directionlist))
                  (cond
                    ((and (even? (quotient ticks 20))
                          (send left-tile passable?)
                          (not (eq? (unbox last-stepped-on) left-tile)))
                     (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))
                    ((and (send right-tile passable?)
                          (not (eq? (unbox last-stepped-on) right-tile)))
                     (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist)))
                    ((and (send left-tile passable?)
                          (not (eq? (unbox last-stepped-on) left-tile)))
                     (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist))))))
            ;------down--------
            (when (< distance-down-sqrd
                     distance-to-player-sqrd)
              (if (and (send down-tile passable?)
                       (not (eq? (unbox last-stepped-on) down-tile)))
                  (set! directionlist (cons (cons distance-down-sqrd 'down) directionlist))
                  (cond
                    ((and (even? (quotient ticks 20))
                          (send left-tile passable?)
                          (not (eq? (unbox last-stepped-on) left-tile)))
                     (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))
                    ((and (send right-tile passable?)
                          (not (eq? (unbox last-stepped-on) right-tile)))
                     (set! directionlist (cons (cons distance-right-sqrd 'right) directionlist)))
                    ((and (send left-tile passable?)
                          (not (eq? (unbox last-stepped-on) left-tile)))
                     (set! directionlist (cons (cons distance-left-sqrd 'left) directionlist)))))))
          (begin
            (set-box! chasing #f)
            (case direction-int
              ((0) (set! directionlist (cons (cons 0 'left) '())))
              ((1) (set! directionlist (cons (cons 0 'right) '())))
              ((2) (set! directionlist (cons (cons 0 'up) '())))
              ((3) (set! directionlist (cons (cons 0 'down) '()))))))
      
      (if (not (null? directionlist))
          (begin (set-box! last-stepped-on
                           (send (send world get-current-map) gettile (send monster getx) (send monster gety)))
                 (send monster move! (cdr (argmin car directionlist)))
                 (set-box! last-moved ticks))
          (begin (set-box! last-stepped-on
                           (send (send world get-current-map) gettile (send monster getx) (send monster gety)))
                 (send monster move! 'stay)
                 (set-box! last-moved ticks))))))