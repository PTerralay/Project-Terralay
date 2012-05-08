#lang racket
(require sgl/gl
         sgl/gl-vectors
         racket/mpair
         racket/gui
         "Map.rkt"
         "Player.rkt"
         "Menu.rkt"
         "Character.rkt"
         "Inventory.rkt"
         "Thing.rkt")

(provide World%)
;____________________________________________________________________________________________________________
;class: Object
;
;Desc: This is the object that is the world. A world contains maps, agents and the player.
;A world also has a state-variable wich tells us what has happened in the world and what has yet to happen.
;we also load the graphics to the world so that we have them predefined, this helps the game run smoother.
;____________________________________________________________________________________________________________

(define World%
  (class object%
    (super-new)
    
    (init-field maplist
                current-map
                state
                canvas)
    
    
    (field (tilegraphics '())
           (chars '())
           (things '())
           (agents (mappend things chars))
           (player (instantiate Player% 
                     (32 32 1 1 'right this canvas 8 (new Inventory% (things '()) (width 5)  (height 3))))))
    
    ;----------------------------------------------------------------
    ;change the state of the world.
    ;params: new-state - the new state we will set to.
    ;----------------------------------------------------------------
    (define/public (set-state! new-state)
      (set! state new-state))
    
    
    ;-----------------------------------------------------------------------
    ;this function changes the current map to the provided one,
    ;we also load all the neighbours to the provided map
    ;params: arg - a variable wich decides wich map we change to.
    ;------------------------------------------------------------------------
    (define/public (set-current-map! arg)
      (if (eq? arg 'first)
          (set! current-map (car maplist))
          (when (findf (lambda (map)
                         (eqv? (get-field mapID map)  arg))
                       maplist)
            (set! current-map (findf (lambda (map)
                                       (display "set current-map to")
                                       (display (get-field mapID map))
                                       (display "#\n")
                                       (eqv? (get-field mapID map) arg))
                                     maplist))))
          (add-neighbours! current-map))
    
    
    ;------------------------------------------------------------------------
    ; add a new map to the map-list
    ; params; new-map - the map we want to add.
    ;------------------------------------------------------------------------
    (define/public (add-map! new-map)
      (set! maplist (cons new-map maplist)))
    
    
    
    ;------------------------------------------------------------------------
    ; add stuff to the list of things and update agents.
    ; params: thing-list - list of things we want to add.
    ;------------------------------------------------------------------------
    (define/public (add-things! thing-list)
      (mfor-each (lambda (thing)
                   (set! things (mcons thing things)))
                 thing-list)
      (set! agents (mappend things chars)))
    
    ;------------------------------------------------------------------------
    ;add the neighbours of a map to the map-list
    ;params: map - the map whose neighbours we want to add.
    ;------------------------------------------------------------------------
    (define/public (add-neighbours! map)
      (display "added neighbours: ")
      (for-each (lambda (element)
                  (display (car element))
                  (display " ")
                  (add-map! (load&create-map (car element) (cadr element) this)))
                (get-field neighbours map))
      (newline))
    
    ;------------------------------------------------------------------------------------------------
    ;this is how we move between the maps, set the current map to chosen neighbour,
    ;then we move the player to the place where the door is in the new map and make him face the room.
    ;If the player is chased the monsters chasing him will eventually be moved to the new room as well.
    ;params: mapname - the name of the new map
    ;        door-exit- x/y - position af the door wich we are moving to.
    ;        exit-dir - the direction we are looking when exiting the door.
    ;-------------------------------------------------------------------------------------------------
    (define/public (map-change! mapname door-exit-x door-exit-y exit-dir)
      (send player set-pos! door-exit-x door-exit-y)
      (send player set-dir! exit-dir)
      
      (mfor-each 
       (lambda (char)
         (when (send char chasing?)
           (new timer% 
                [notify-callback 
                 (lambda ()
                   (when (and (eqv? (get-field mapID current-map) mapname)
                              (not (eqv? (get-field place char)
                                         (get-field mapID current-map))))
                     (send char set-pos! door-exit-x door-exit-y)
                     (send char setplace! mapname)))]
                [interval (+ (sqr (- (get-field gridx player) (get-field gridx char)))
                             (sqr (- (get-field gridy player) (get-field gridy char)))
                             1000)]
                [just-once? #t])))
       chars)
      (set-current-map! mapname))
    
    
    ;-------------------------------------------------------------------------------------------------
    ;the characters are simply instansiated and saved to the list of characters and update agents.
    ;params: datalist - a list of all characters that are to be loaded
    ;-------------------------------------------------------------------------------------------------
    (define/public (character-load datalist)
      
      (define (load-loop charlist)
        (if (null? charlist)
            '()
            (let ((new-char (new Character%
                                 (gridx (dynamic-require (cdar charlist) 'GX))
                                 (gridy (dynamic-require (cdar charlist) 'GY))
                                 (triggerlist (dynamic-require (cdar charlist) 'triggers))
                                 (AI-update (dynamic-require (cdar charlist) 'AI))
                                 (interaction (dynamic-require (cdar charlist) 'interact-code))
                                 (agent-ID (dynamic-require (cdar charlist) 'ID))
                                 (world this)
                                 (place (dynamic-require (cdar charlist) 'placement))
                                 (state (dynamic-require (cdar charlist) 'state))
                                 (type (dynamic-require (cdar charlist) 'type)))))
              
              (mcons new-char(load-loop (cdr charlist))))))
      (set! chars (load-loop datalist))
      (set! agents (mappend things chars)))
    
    ;------------------------------------------------------------------------------
    ;savegame: writes Data to a file with the name filename
    ;params:
    ; filename - a string that is the path to the new savefile. 
    ;------------------------------------------------------------------------------
    (define/public (savegame filename)
      (let ((savefile (open-output-file filename #:mode 'binary #:exists 'truncate))
            (Data '()))
        
        (define (saveagent agent)
          (set! Data (cons (cons (get-field type agent)
                                 (list
                                  (list (cons 'name (get-field agent-ID agent))
                                        (cons 'place (get-field place agent))
                                        (cons 'gridx (get-field gridx agent))
                                        (cons 'gridy (get-field gridy agent))
                                        (cons 'state (get-field state agent))
                                        (cons 'type (get-field type agent))
                                        ))) Data)))
        (define (saveplayer)
          (set! Data (cons (cons 'player
                                 (list
                                  (list (cons 'gridx (get-field gridx player))
                                        (cons 'gridy (get-field gridy player))
                                        (cons 'dir (get-field dir player))
                                        ))) Data)))
        (define (save-the-world)
          (set! Data (cons (cons 'world
                                 (list
                                  (list (cons 'current-map (get-field mapID current-map))
                                        (cons 'state state))))
                           Data)))
        
        (mfor-each (λ (agent)
                     (saveagent agent)) agents)
        (saveplayer)
        (save-the-world)
        (write Data savefile)
        (close-output-port savefile)))
    
    ;------------------------------------------------------------------------------
    ; Loadgame
    ; Desc: simply tells the game to load the chosen save.
    ; filename - the file from wich we will load the game.
    ;------------------------------------------------------------------------------
    (define/public (loadgame filename)
      (let* ((loadfile (open-input-file filename #:mode 'binary))
             (loaddata (read loadfile)))
        
        (define (loop loadlist)
          (if (null? loadlist)
              (close-input-port loadfile)
              (let ((element (car loadlist)))
                (case (car element)
                  ((world) (set! state (cdr (assq 'state (cadr element))))
                           (let ((map-pair (assq (cdr (assq 'current-map (cadr element))) (dynamic-require "Gamedata/MapIndex.rkt" 'Index))))
                             (set! current-map (load&create-map (car map-pair) (cdr map-pair) this))
                             (add-neighbours! current-map)))
                  ((monster)
                   (let* ((monsterfile (cdr (assq (cdr (assq 'name (cadr element)))
                                                  (dynamic-require "Gamedata/Agentdata.rkt" 'Character-list)))))
                     
                     (set! chars (mcons (new Character%
                                             (gridx (cdr (assq 'gridx (cadr element))))
                                             (gridy (cdr (assq 'gridy (cadr element))))
                                             (triggerlist (dynamic-require monsterfile 'triggers))
                                             (AI-update (dynamic-require monsterfile 'AI))
                                             (interaction (dynamic-require monsterfile 'interact-code))
                                             (agent-ID (cdr (assq 'name (cadr element))))
                                             (world this)
                                             (place (cdr (assq 'place (cadr element))))
                                             (state (cdr (assq 'state (cadr element))))
                                             (type (cdr (assq 'type (cadr element)))))
                                        chars))))
                  
                  ((thing)
                   (let* ((thingdata (dynamic-require "Gamedata/Agentdata.rkt" (cdr (assq 'name (cadr element)))))
                          (new-thing (new Thing%
                                          (gridx (cdr (assq 'gridx (cadr element))))
                                          (gridy (cdr (assq 'gridy (cadr element))))
                                          (triggerlist (cdr (assq 'triggers thingdata)))
                                          (interaction (cdr (assq 'interaction-code thingdata)))
                                          (world this)
                                          (agent-ID (cdr (assq 'name (cadr element))))
                                          (inv-name (cdr (assq 'inv-name thingdata)))
                                          (place (cdr (assq 'place (cadr element))))
                                          (state (cdr (assq 'state (cadr element))))
                                          (type (cdr (assq 'type (cadr element)))))))
                     (when (eqv? (cdr (assq 'place (cadr element))) 'Inventory)
                       (send (get-field inventory player) add-thing! new-thing))
                     
                     (set! things (mcons new-thing things))))
                  
                  ((player) (set! player 
                                  (new Player% 
                                       (xpos (* 32 (cdr (assq 'gridx (cadr element)))))
                                       (ypos (* 32 (cdr (assq 'gridy (cadr element)))))
                                       (gridx (cdr (assq 'gridx (cadr element))))
                                       (gridy (cdr (assq 'gridy (cadr element))))
                                       (dir (cdr (assq 'dir (cadr element))))
                                       (world this)
                                       (glcanvas canvas)
                                       (speed 8)
                                       (inventory (new Inventory% (things '()) (width 5)  (height 3))))))
                  
                  )
                (loop (cdr loadlist)))))
        (display "clearing cache\n")
        (set! maplist (void))
        (set! chars '())
        (set! player (void))
        (set! things '())
        (set! agents '())
        (display "recreating the world\n")
        (loop loaddata)
        (set! agents (mappend things chars))))
    ))




