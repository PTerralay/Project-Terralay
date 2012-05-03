#lang racket

(require "Agent.rkt")
(provide Thing% Load-things)
;-----------------------------------------------------------------------------------
;Class: Agent
;Desc: this is a thing, can be any object in the world that can be interacted with,
;but cannot interact with stuff itself
;-----------------------------------------------------------------------------------
(define Thing%
  (class Agent%
    (super-new)
    (inherit-field xpos
                   ypos 
                   gridx
                   gridy
                   triggerlist
                   agent-ID
                   place
                   world
                   interaction
                   state)
    (init-field inv-name)
    
    ;-----------------------------------------------------------------------------------
    ; runs the interaction-code defined in the thing's datafile
    ;-----------------------------------------------------------------------------------
    (define/public (interact)
      (interaction world this))
    
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


;-----------------------------------------------------------------------------------
; sets thing-list to be a list with things instansiated here.
;params: thing-list - a list of names of the things that are to be loaded from the Agentdata file.
;        the-world - the world to wich the things are to be loaded.
;-----------------------------------------------------------------------------------
(define (Load-things thing-list the-world)
  (define (createloop thing-name)
    (let* ((data (dynamic-require "Gamedata/Agentdata.rkt" thing-name)))
      (new Thing%
           (gridx (cdr (assq 'GX data)))
           (gridy (cdr (assq 'GY data)))
           (triggerlist (cdr (assq 'triggers data)))
           (interaction (cdr (assq 'interaction-code data)))
           (world the-world)
           (agent-ID thing-name)
           (inv-name (cdr (assq 'inv-name data)))
           (place (cdr (assq 'placement data)))
           (state (cdr (assq 'state data)))
           (type (cdr (assq 'type data))))))
  
  (if (null? thing-list)
      '()
      (mcons (createloop (car thing-list)) (Load-things (cdr thing-list) the-world))))