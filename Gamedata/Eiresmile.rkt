#lang racket

(provide (all-defined-out))


(define state 3)
(define type 'monster)
(define GX 16)
(define GY 4)
(define X (* 32 GX))
(define Y (* 32 GY))
(define ID 'Eiresmile)
(define tex-ID 2)
(define tex-Width 32)
(define tex-Height 64)
(define speed 3)
(define placement 'Limbo)
(define passable? #f)

(define triggers (list
                  ))
(define (interact-code lst)
  (display "Nom!"))


(define (movecondition world monster)
  (let* ((target-x
          (get-field gridx (get-field player world)))
         (target-y
          (get-field gridy (get-field player world)))
         (distance-to-target-sqrd
          (+ (sqr (- target-x (get-field gridx monster))) 
             (sqr (- target-y (get-field gridy monster)))))
         (threshhold 200)
         (pred #t))
    
    (if (and (eq? target-x (get-field gridx monster))
             (eq? target-y (get-field gridy monster)))
        (set-field! state world -1)  
        (if (< distance-to-target-sqrd threshhold)
            'move
            (lambda (world monster)
              (void))))))