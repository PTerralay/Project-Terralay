#lang racket

(require "Agent.rkt" "world.rkt" "Map.rkt" "Tile.rkt")

(provide Character% AI-loop)

(define Character%
  (class Agent%
    (super-new)
    (inherit-field xpos ypos)
    (define/public (talk-to) "not implemented yet")
    (define/public (move! direction)
      (case direction
        ((up) (when (send (send (send Trollworld get-current-map) gettile xpos (- ypos 1)) passable?) (set! ypos (- ypos 1)))) ; need to be able to ask neighbouring tile if passable.
        ((down) (when (send (send (send Trollworld get-current-map) gettile xpos (+ ypos 1)) passable?) (set! ypos (+ ypos 1))))
        ((left) (when (send (send (send Trollworld get-current-map) gettile (- xpos 1) ypos) passable?) (set! xpos (- xpos 1))))
        ((right) (when (send (send (send Trollworld get-current-map) gettile (+ xpos 1) ypos) passable?) (set! xpos (+ xpos 1))))))
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

(define (CreateChar world name x y) ;create a character, Duh!
  (send (send world get-current-map) Add-agent! name)
  (new Character% (xpos x) (ypos y)))

(define testmap (Load&Create 'testmap "Loadtest.txt"))
(define Trollworld (new World% (maplist '(testmap)) (current-map testmap) (state 1)))

(define Gustaf (CreateChar Trollworld 'gustaf 5 3))
