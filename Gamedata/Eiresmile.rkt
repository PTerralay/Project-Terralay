#lang racket

(provide X Y GX GY triggers movecondition interact-code ID placement state type tex-ID)


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
