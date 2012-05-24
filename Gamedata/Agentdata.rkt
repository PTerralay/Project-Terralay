#lang racket/gui
(require "../Thing.rkt")
(provide (all-defined-out))

(define Screwdriver
  (list
   (cons 'GX 3)
   (cons 'GY 4)
   (cons 'tex-ID 2)
   (cons 'inv-name "Screwdriver")
   (cons 'triggers '())
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'interaction-code
         (λ (world self use-with)
           (when (null? use-with)
             (set-field! place self 'Inventory)
             (send world draw-text-ingame
                   'outside_workroom
                   -3 
                   3
                   0.6
                   "A screwdriver, maybe I can use this for something"
                   200))))
   (cons 'placement 'outside_workroom)
   (cons 'state 0)
   (cons 'passable? #t)))

(define Generator1
  (list
   (cons 'GX 4)
   (cons 'GY 11)
   (cons 'tex-ID 0)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Generator1")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (if (null? use-with)
               (send world draw-text-ingame
                     'Engineroom1
                     -2
                     11
                     0.6
                     "The generator is broken, seems rodents are to blame..."
                     200)
               (when (eqv? (get-field agent-ID use-with) 'Screwdriver)
                 (begin (set-field! place use-with 'Limbo)
                        (set-field! state world 3)
                        (send world draw-text-ingame
                              'Engineroom1
                              -2
                              11
                              0.6
                              "There, the cable is fixed, for now..."
                              200))))))
   (cons 'placement 'Engineroom1)
   (cons 'state 3)
   (cons 'passable? #f)))

(define Generator2
  (list
   (cons 'GX 3)
   (cons 'GY 11)
   (cons 'tex-ID 1)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Generator2")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (if (null? use-with)
               (send world draw-text-ingame
                     'Engineroom1
                     -2
                     11
                     0.6
                     "the generator is broken, seems rodents are to blame..."
                     200)
               (when (eqv? (get-field agent-ID use-with) 'Screwdriver)
                 (begin (set-field! place use-with 'Limbo)
                        (new timer%
                             (interval 2000)
                             (just-once? #t)
                             (notify-callback
                              (λ ()
                                (set-field! state world 3))))
                        (send world draw-text-ingame
                              'Engineroom1
                              -2
                              11
                              0.6
                              "there, the cable is fixed, for now..."
                              200))))))
   (cons 'placement 'Engineroom1)
   (cons 'state 3)
   (cons 'passable? #f)))

(define WRDesk
  (list
   (cons 'GX 2)
   (cons 'GY 3)
   (cons 'tex-ID 4)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -45)
   (cons 'inv-name "Desk")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Workroom
                 -2
                 -2
                 0.6
                 "A mess of jumbled calculations and theorems,\n  unintelligble for anyone but you."
                 300)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define WRChair
  (list
   (cons 'GX 3)
   (cons 'GY 2)
   (cons 'tex-ID 5)
   (cons 'tex-width 32)
   (cons 'tex-height 50)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Chair")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (void)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define O_WRDesk
  (list
   (cons 'GX 2)
   (cons 'GY 11)
   (cons 'tex-ID 4)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -45)
   (cons 'inv-name "Desk")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'outside_workroom
                 -2
                 9
                 0.6
                 "A mess of calculations and theorems,\n  Who wrote this crap?"
                 300)))
   (cons 'placement 'outside_workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define ODesk
  (list
   (cons 'GX 2)
   (cons 'GY 5)
   (cons 'tex-ID 4)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -45)
   (cons 'inv-name "Desk")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Office1
                 -2
                 -1
                 0.6
                 "Seems to be a log for some experiment.\n  Project Terralay research log entry #607:\n  Today is the day we are giong to test the relay, the others are firing up the heater outside,\n  while I'm preparing the sample.\n  I have to say I've never seen anyhing like it, it glows with a wierd dim light\n  as if it was from another world.\n  I can't shake the feeling that there is something inside it looking out at me..."
                 600)))
   (cons 'placement 'Office1)
   (cons 'state 3)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define OChair
  (list
   (cons 'GX 3)
   (cons 'GY 4)
   (cons 'tex-ID 5)
   (cons 'tex-width 32)
   (cons 'tex-height 50)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Chair")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (void)))
   (cons 'placement 'Office1)
   (cons 'state 3)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define SlidedoorLclosed
  (list
   (cons 'GX 24)
   (cons 'GY 4)
   (cons 'tex-ID 3)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Slidedoor")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'outside_workroom
                 20
                 3
                 0.6
                 "The elevator door is closed, I need to get the power running.\n I can hear some strange noises behind the door though..."
                 200)))
   (cons 'placement 'outside_workroom)
   (cons 'state 3)
   (cons 'passable? #f)))

(define Testthing
  (list
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'tex-ID 1)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Trol")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Anotherawesomeroom
                 19
                 2
                 0.6
                 "DON'T TOUCH ME!!!"
                 200)))
   (cons 'placement 'Anotherawesomeroom)
   (cons 'state 0)
   (cons 'passable? #t)))

(define Statebutton
  (list
   (cons 'GX 4)
   (cons 'GY 7)
   (cons 'tex-ID 1)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Button")
   (cons 'triggers '())
   (cons 'placement 'Awesomeroom)
   (cons 'interaction-code
         (λ (world thing use-with)
           (if (eq? (get-field masked world) #t)
               (begin (set-field! masked world #f)
                      (send world draw-text-ingame
                            'Anotherawesomeroom
                            4
                            6
                            0.6
                            "Oh!"
                            50))
               (begin (set-field! masked world #t)
                      (send world draw-text-ingame
                            'Anotherawesomeroom
                            4
                            6
                            0.6
                            "Oh!"
                            50)))))
   (cons 'state 0)
   (cons 'passable? #f)))

(define WRServer1
  (list
   (cons 'GX 6)
   (cons 'GY 2)
   (cons 'tex-ID 6)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Server")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Workroom
                 5
                 -2
                 0.6
                 "It's a hitech server, normally running complex calculations"
                 300)))
   (cons 'placement 'Workroom)
   (cons 'state 1)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define WRServer2
  (list
   (cons 'GX 7)
   (cons 'GY 2)
   (cons 'tex-ID 6)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Server")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Workroom
                 5
                 -2
                 0.6
                 "It's a hitech server, normally running complex calculations"
                 300)))
   (cons 'placement 'Workroom)
   (cons 'state 1)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define WRServer3
  (list
   (cons 'GX 8)
   (cons 'GY 2)
   (cons 'tex-ID 6)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Server")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Workroom
                 5
                 -1
                 0.6
                 "It's a hitech server, normally running complex calculations"
                 300)))
   (cons 'placement 'Workroom)
   (cons 'state 1)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define WRwb1
  (list
   (cons 'GX 6)
   (cons 'GY 7)
   (cons 'tex-ID 7)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Workbench")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (if (null? use-with)
               (send world draw-text-ingame
                     'Workroom
                     -2
                     11
                     0.6
                     "It's my workbench."
                     100)
               (when (eqv? (get-field agent-ID use-with) 'Amulet-piece)
                 (begin (set-field! place use-with 'Limbo)
                        (set-field! state world 2)
                        (send world draw-text-ingame
                              'Workroom
                              -2
                              11
                              0.6
                              "All done! Now i'll have a never-ending supply of light around my neck\n Too bad it only shines in front of me though..."
                              300)
                        (new timer%
                             (interval 6000)
                             (just-once? #t)
                             (notify-callback (λ ()
                                                (set-field! masked world #t)
                                                (set-field! state world 2))))
                        (new timer%
                             (interval 7000)
                             (just-once? #t)
                             (notify-callback
                              (λ ()
                                (send world draw-text-ingame
                                      'Workroom
                                      4
                                      11
                                      0.6
                                      "What?! The lights went out, and the servers stopped humming...\n A power outage here? That can't be possible..."
                                      200))))
                        (new timer%
                             (interval 12000)
                             (just-once? #t)
                             (notify-callback
                              (λ ()
                                (send world draw-text-ingame
                                      'Workroom
                                      4
                                      11
                                      0.6
                                      "I better try to find a way to fire up the backup generators,\n I seem to be the only one left in this complex today."
                                      300)))))))))
   (cons 'placement 'Workroom)
   (cons 'state 1)
   (cons 'passable? #f)))

(define WRwb2
  (list
   (cons 'GX 7)
   (cons 'GY 7)
   (cons 'tex-ID 8)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Workbench")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (if (null? use-with)
               (send world draw-text-ingame
                     'Workroom
                     -2
                     11
                     0.6
                     "It's my workbench."
                     100)
               (when (eqv? (get-field agent-ID use-with) 'Amulet-piece)
                 (begin (set-field! place use-with 'Limbo)
                        (set-field! state world 2)
                        (send world draw-text-ingame
                              'Workroom
                              1
                              9
                              0.6
                              "All done! Now i'll have a never-ending supply of light around my neck\n Too bad it only shines in front of me though..."
                              300)
                        (new timer%
                             (interval 6000)
                             (just-once? #t)
                             (notify-callback
                              (λ ()
                                (set-field! masked world #t)
                                (set-field! state world 2))))
                        (new timer%
                             (interval 7000)
                             (just-once? #t)
                             (notify-callback
                              (λ ()
                                (send world draw-text-ingame
                                      'Workroom
                                      4
                                      9
                                      0.6
                                      "What?! The lights went out, and the servers stopped humming...\n A power outage here? That can't be possible..."
                                      300))))
                        (new timer%
                             (interval 13000)
                             (just-once? #t)
                             (notify-callback
                              (λ ()
                                (send world draw-text-ingame
                                      'Workroom
                                      4
                                      9
                                      0.6
                                      "I better try to find a way to fire up the backup generators,\n I seem to be the only one left in this complex today."
                                      300)))))))))
   (cons 'placement 'Workroom)
   (cons 'state 1)
   (cons 'passable? #f)))

(define Amulet-piece
  (list
   (cons 'GX -1)
   (cons 'GY -1)
   (cons 'tex-ID 2)
   (cons 'inv-name "Unfinished\n Amulet")
   (cons 'triggers '())
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'interaction-code
         (λ (world self use-with)
           (void)))
   (cons 'placement 'Inventory)
   (cons 'state 1)
   (cons 'passable? #t)))


(define RelayPillar1
  (list
   (cons 'GX 6)
   (cons 'GY 8)
   (cons 'tex-ID 9)
   (cons 'tex-width 64)
   (cons 'tex-height 96)
   (cons 'tex-rel-x -32)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Pillar")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Relayroom
                 (- (get-field gridx self) 2)
                 (- (get-field gridy self) 2)
                 0.6
                 "It seems to be some kind of powersource..."
                 300)))
   (cons 'placement 'Relayroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define RelayPillar2
  (list
   (cons 'GX 17)
   (cons 'GY 5)
   (cons 'tex-ID 9)
   (cons 'tex-width 64)
   (cons 'tex-height 96)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Pillar")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Relayroom
                 (- (get-field gridx self) 2)
                 (- (get-field gridy self) 2)
                 0.6
                 "It seems to be some kind of powersource..."
                 300)))
   (cons 'placement 'Relayroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define BRelayPillar1
  (list
   (cons 'GX 6)
   (cons 'GY 5)
   (cons 'tex-ID 10)
   (cons 'tex-width 64)
   (cons 'tex-height 96)
   (cons 'tex-rel-x -32)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Pillar")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Relayroom
                 (- (get-field gridx self) 2)
                 (- (get-field gridy self) 2)
                 0.6
                 "It's broken. But these holes seems unnatural..."
                 300)))
   (cons 'placement 'Relayroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define BRelayPillar2
  (list
   (cons 'GX 17)
   (cons 'GY 8)
   (cons 'tex-ID 10)
   (cons 'tex-width 64)
   (cons 'tex-height 96)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Pillar")
   (cons 'triggers '())
   (cons 'interaction-code
         (λ (world self use-with)
           (send world draw-text-ingame
                 'Relayroom
                 (- (get-field gridx self) 2)
                 (- (get-field gridy self) 2)
                 0.6
                 "It's broken. But these holes seems unnatural..."
                 300)))
   (cons 'placement 'Relayroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")
   (cons 'Eiresmile "Gamedata/Eiresmile.rkt")
   ;(cons 'Curious "Gamedata/Curious.rkt")
   ;Curious isn't allowed to play because he just breaks the game.
   ))

(define Thing-list
  (list 'Testthing
        'Statebutton
        'Generator1
        'Generator2
        'Screwdriver
        'SlidedoorLclosed
        'WRDesk
        'WRChair
        'WRServer1
        'WRServer2
        'WRServer3
        'WRwb1
        'WRwb2
        'Amulet-piece
        'OChair
        'ODesk
        'RelayPillar1
        'RelayPillar2
        'BRelayPillar1
        'BRelayPillar2
        'O_WRDesk))
