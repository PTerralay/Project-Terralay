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
        ((up) (when (send (send (send Trollworld get-current-map) gettile gridx (- gridy 1)) passable?) 
                (set! gridy (- gridy 1)))) 
        ((down) (when (send (send (send Trollworld get-current-map) gettile gridx (+ gridy 1)) passable?) 
                  (set! gridy (+ gridy 1))))
        ((left) (when (send (send (send Trollworld get-current-map) gettile (- gridx 1) gridy) passable?) 
                  (set! gridx (- gridx 1))))
        ((right) (when (send (send (send Trollworld get-current-map) gettile (+ gridx 1) gridy) passable?) 
                   (set! gridx (+ gridx 1))))))
    (define/public (gety)
      gridy)
    (define/public (getx)
      gridx)
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
