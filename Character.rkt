#lang racket

(require "Agent.rkt")

(provide Character% AI-loop)

(define Character%
  (class Agent%
    (super-new)
    (inherit-field xpos ypos)
    (define/public (talk-to) "not implemented yet")
    (define/public (move! direction)
      (case direction
        ((up) (set! ypos (+ ypos 1))) ; need to be able to ask neighbouring tile if passable.
        ((down) (set! ypos (- ypos 1)))
        ((left) (set! xpos (- xpos 1)))
        ((right) (set! xpos (+ xpos 1)))))
    (define/public (gety)
      ypos)
    (define/public (getx)
      xpos)
    ))


;; Faaar from complete, add limiters and some kind of timer

(define (AI-loop character)
  (let ((dir-num (random 4)))
    (send character move! (case dir-num
                            ((0) 'up)
                            ((1) 'right)
                            ((2) 'down)
                            ((3) 'left)
                            ((4) 'stay)))
  (AI-loop character)))
;-----------------------------------------------------------;

(define (CreateChar name x y) ;create a character, Duh!
  (new Character% (xpos x) (ypos y)))