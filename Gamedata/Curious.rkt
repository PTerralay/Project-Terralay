#lang racket

(provide X Y GX GY triggers movecondition interact-code ID placement state type tex-ID speed tex-Width tex-Height passable?)



(define state 2)
(define type 'monster)
(define GX 4)
(define GY 4)
(define X (* 32 GX))
(define Y (* 32 GY))
(define tex-Width 32)
(define tex-Height 64)
(define ID 'Curious)
(define tex-ID 3)
(define speed 4)
(define passable? #t)
(define placement 'Relayroom)

(define triggers (list
                  ))

(define (interact-code cmd)
  (display "Nom!"))

(define (movecondition world monster)
  (let* ((target-x (get-field gridx (get-field player world)))
         (target-y (get-field gridy (get-field player world)))
         (distance-to-target-sqrd (+ (sqr (- target-x (get-field gridx monster))) 
                                     (sqr (- target-y (get-field gridy monster)))))
         (threshhold 100)
         (pred #t))
    
    (if (or (<= (abs (- target-x (get-field gridx monster))) 3)
            (<= (abs (- target-y (get-field gridy monster))) 3))
        'stay
        (if (and (> distance-to-target-sqrd threshhold)
                 (or (and (eqv? (get-field dir (get-field player world)) 'left)
                          (< (get-field gridx monster) (get-field gridx (get-field player world))))
                     (and (eqv? (get-field dir (get-field player world)) 'right)
                          (> (get-field gridx monster) (get-field gridx (get-field player world))))
                     (and (eqv? (get-field dir (get-field player world)) 'up)
                          (< (get-field gridy monster) (get-field gridy (get-field player world))))
                     (and (eqv? (get-field dir (get-field player world)) 'down)
                          (> (get-field gridx monster) (get-field gridx (get-field player world))))))
            
            (lambda (world monster)
              (let ((left (send (get-field current-map world) gettile
                                 (- (get-field gridx (get-field player world)) 5)
                                 (get-field gridy (get-field player world))))
                    (right (send (get-field current-map world) gettile
                                 (+ (get-field gridx (get-field player world)) 5)
                                 (get-field gridy (get-field player world))))
                    (up (send (get-field current-map world) gettile
                                 (get-field gridx (get-field player world))
                                 (- (get-field gridy (get-field player world)) 5)))
                    (down (send (get-field current-map world) gettile
                                 (get-field gridx (get-field player world))
                                 (+ (get-field gridy (get-field player world)) 5))))
              (cond 
                ((get-field passable left) (send monster set-pos! (get-field gridx left) (get-field gridy monster)))
                ((get-field passable right) (send monster set-pos! (get-field gridx right) (get-field gridy monster)))
                ((get-field passable up) (send monster set-pos! (get-field gridx monster) (get-field gridy up)))
                ((get-field passable down) (send monster set-pos! (get-field gridx monster) (get-field gridy down))))))
            'move))))