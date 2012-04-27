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
                             (if (eq? (send thing get-place) 'Inventory)
                                 (void)
                                 (when (eq? (send world get-state) 0)
                                   (send world set-state! 1)
                                   (send (send (send world get-player) get-inventory) add-thing! thing)
                                   (send thing set-place! 'Inventory)))))))

(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")))

(define Thing-list
  (list 'Testthing
        'Statebutton))