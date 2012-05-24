#lang racket

(provide (all-defined-out))

(define state 0)
(define type 'monster)
(define GX 35)
(define GY 4)
(define X (* 32 GX))
(define Y (* 32 GY))
(define ID 'Tetsytoo)
(define tex-ID 1)
(define tex-Width 32)
(define tex-Height 64)
(define speed 4)
(define placement 'Anotherawesomeroom)
(define passable? #t)

(define triggers
  (list
   (list
    (cons 'poll (lambda (char world)
                  (and (eq? (get-field gridx (get-field player world))
                            (get-field gridx char))
                       (eq? (get-field gridy (get-field player world))
                            (get-field gridy char)))))
    (cons 'act (lambda (char world)
                 (display "GOTCHA YOU PRETTY PRETTY LITTLE GIRL! Now come here...\n"))))))

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