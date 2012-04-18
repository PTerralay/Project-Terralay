#lang racket

(provide Testthing Character-list Statebutton)

(define Testthing
  (list
   (cons 'X 672)
   (cons 'Y 96)
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'triggers '())
   (cons 'interaction-code (lambda () 
                             (display "Don't touch me!")))
   (cons 'placement 'Awesomeroom)))

(define Statebutton
  (list
   (cons 'GX 11)
   (cons 'GY 9)
   (cons 'triggers '())
   (cons 'placement 'Awesomeroom)
   (cons 'interaction-code (lambda (world thing)
                             (when (eq? (send world get-state) 0)
                               (send world set-state! 1)
                               (display "Button pressed, a door opens"))))))

(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")))