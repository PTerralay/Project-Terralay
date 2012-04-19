#lang racket

(provide Testthing Character-list Statebutton Thing-list)

(define Testthing
  (list
   (cons 'X 672)
   (cons 'Y 96)
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self)
                             (display "Don't touch me!")))
   (cons 'placement 'Anotherawesomeroom)))

(define Statebutton
  (list
   (cons 'X 352)
   (cons 'Y 288)
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

(define Thing-list
  (list 'Testthing
        'Statebutton
        ))