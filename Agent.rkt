#lang racket

(provide Agent%)
;-------------------------------------------------------------------------------
;
;class: Object
;Desc: this is an agent, it is either a character or a thing.
;An agent has information on where it is and what it does when interacted with.
;-------------------------------------------------------------------------------

(define Agent%
  (class object%
    (super-new)
    (init-field gridx
                gridy
                triggerlist
                world
                place
                agent-ID
                interaction
                state
                tex-ID
                passable)
    
    (field (xpos (* gridx 32))
           (ypos (* gridy 32)))))