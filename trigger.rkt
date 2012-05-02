#lang racket

(provide Trigger%)

;------------------------------------------------------------------------------
;Class: Trigger%
;Description: A Trigger% object simply contains a polling function that determines whether
; or not the trigger is active, and a acting function that carries out an action.
;------------------------------------------------------------------------------
(define Trigger%
  (class object%
    (super-new)
    (init-field trigger-assoc)
    (define poll-fn (cdr (assq 'poll trigger-assoc)))
    (define act-fn (cdr (assq 'act trigger-assoc)))
    
    
    ;------------------------------------------------------------------------------
    ;poll&act: runs the polling function and calls the acting function if it returns true.
    ;params:
    ; obj - the object holding the trigger
    ; world - the entire world of the game
    ;------------------------------------------------------------------------------
    (define/public (poll&act obj world)
        (when (poll-fn obj  world)
          (act-fn obj world)))))
