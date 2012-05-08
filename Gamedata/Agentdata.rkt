#lang racket
(require "../Thing.rkt")
(provide Testthing Character-list Statebutton Thing-list)

(define Testthing
  (list
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'inv-name "Trol")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self)
                             (display "Don't touch me!")))
   (cons 'placement 'Anotherawesomeroom)
   (cons 'state 0)
   (cons 'type 'thing)))

(define Statebutton
  (list
   (cons 'GX 11)
   (cons 'GY 9)
   (cons 'inv-name "Button")
   (cons 'triggers '())
   (cons 'placement 'Awesomeroom)
   (cons 'interaction-code (lambda (world thing)
                             (if (eq? (get-field place thing) 'Inventory)
                                 (void)
                                 (begin (send world set-state! 1)
                                        (send (get-field inventory (get-field player world)) add-thing! thing)
                                        (send thing set-place! 'Inventory)))))
   (cons 'state 0)
   (cons 'type 'thing)))

(define Character-list
  (list 
   ;(cons 'Tetsy "Gamedata/testmonster.rkt")
   ;(cons 'Tetsytoo "Gamedata/testmonster2.rkt")
   (cons 'Eiresmile "Gamedata/Eiresmile.rkt")))

(define Thing-list
  (list 'Testthing
        'Statebutton))