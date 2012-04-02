(require "Game.rkt" "World.rkt")
(define *world* (new World%
                   (maplist mapplista) 
                   (current-map (car mapplista)) 
                   (state 0)))