#lang racket
(require "../Thing.rkt")
(provide Testthing Character-list Statebutton Thing-list)

(define Testthing
  (list
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self)
                             (display "Don't touch me!")))
   (cons 'placement 'Anotherawesomeroom)))

(define Statebutton
  (list
   (cons 'GX 11)
   (cons 'GY 9)
   (cons 'triggers '())
   (cons 'placement 'Awesomeroom)
   (cons 'interaction-code (lambda (world thing)
                             (when (eq? (send world get-state) 0)
                               (send world set-state! 1)
                               (send (send (send world get-player) get-inventory) add-thing! (new Thing% 
                                                                                                  (gridx -1) 
                                                                                                  (gridy -1) 
                                                                                                  (triggerlist '())
                                                                                                  (world world)
                                                                                                  (agent-ID "Pretty Key")
                                                                                                  (place 'Awesomeroom)
                                                                                                  (interaction (lambda () (void)))))
                                                                                                  
                               (display "You've got the key!"))))))

(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")))

(define Thing-list
  (list 'Testthing
        'Statebutton
        ))