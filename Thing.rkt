#lang racket

(require "Agent.rkt")
(provide Thing%)
;-----------------------------------------------------------------------------------
;Class: Agent
;Desc: this is a thing, can be any object in the world that can be interacted with,
;but cannot interact with stuff itself
;-----------------------------------------------------------------------------------
(define Thing%
  (class Agent%
    (super-new)
    (inherit-field 
     xpos
     ypos 
     gridx
     gridy
     triggerlist
     agent-ID
     place
     world
     interaction
     state
     tex-ID
     passable)
    (init-field inv-name tex-width tex-height tex-rel-x tex-rel-y)
    
    ;-----------------------------------------------------------------------------------
    ; runs the interaction-code defined in the thing's datafile
    ;-----------------------------------------------------------------------------------
    (define/public (interact using)
      (interaction world this using))
    
    ;-----------------------------------------------------------------------------------
    ;set the place of the thing to new-place
    ;-----------------------------------------------------------------------------------
    (define/public (set-place! new-place)
      (set! place new-place))
    
    ;-----------------------------------------------------------------------------------
    ;adds trigger to the triggerlist.
    ;-----------------------------------------------------------------------------------
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))))


