#lang racket

(provide Agent%)
;____________________________________________________________________________________________
;
;class: Object
;Desc: this is an agent, it is either a character or a thing. An agent has information on
;where it is and what it does when interacted with.
;____________________________________________________________________________________________

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
                type
                tex-ID
                tex-Width
                tex-Height)
    
    (field (xpos (* gridx 32))
           (ypos (* gridy 32)))
    
    ;__________________________________________________________________________________________________
    ; a method for when we want to move the character, such as when moving around or moving between maps
    
    (define/public (setplace! new-place)
      (set! place new-place))
    ;__________________________________________________________________________________________________
    ))