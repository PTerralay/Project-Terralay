#lang racket

(provide X Y GX GY triggers movecondition interact-code ID placement state type tex-ID speed  tex-Width tex-Height passable?)


(define state 0)
(define type 'monster)
(define GX 10)
(define GY 4)
(define X (* 32 GX))
(define Y (* 32 GY))
(define ID 'Tetsy)
(define tex-ID 1)
(define tex-Width 32)
(define tex-Height 64)
(define speed 4)
(define placement 'Awesomeroom)
(define passable? #t)

(define triggers (list
                  (list
                   (cons 'poll (lambda (char world)
                                 (and (eq? (get-field gridx (get-field player world)) (get-field gridx char))
                                      (eq? (get-field gridy (get-field player world)) (get-field gridy char)))))
                   (cons 'act (lambda (char world)
                                (display "GOTCHA YOU PRETTY PRETTY LITTLE GIRL! Now come here...\n"))))))
(define (interact-code lst)
  (display "Nom!"))

(define (movecondition world nai-else? monster)
  (let* ((target-x (get-field gridx (get-field player world)))
        (target-y (get-field gridy (get-field player world)))
        (distance-to-target-sqrd (+ (sqr (- target-x (get-field gridx monster))) 
                                    (sqr (- target-y (get-field gridy monster)))))
        (threshhold 75)
        (pred #t))
    (if (not nai-else?)
        (and (< distance-to-target-sqrd threshhold)
             pred)
        (λ ()
          'stay))))