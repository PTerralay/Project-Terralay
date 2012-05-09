;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;

#lang racket/gui

(require "World.rkt" "Player.rkt" "Map.rkt" "Thing.rkt" "Menu.rkt" "drawtext.rkt" "graphics-utils.rkt"
         racket/gui
         (planet "main.rkt" ("clements" "rsound.plt" 3 2))
         racket/mpair
         sgl/gl
         sgl/gl-vectors)


;----------------------------------
;    Module Variables
;----------------------------------
(define backgrounds '())
(define tile-texture-list #f)
(define text-texture-list #f)
(define char-texture-list #f)
(define thing-texture-list #f)
(define texture-list #f)
(define char-animations #f)
(define in-menu #f)
(define in-inventory #f)
(define in-interactions-menu #f)
(define message-list-box (box '{}))


;============================================================================
;                                  Init
;============================================================================

;------------------------------------------------------------------------------
;Class gl-canvas%
;Description: gl-canvas% is a OpenGL-styled canvas% that handles painting and key events.
;------------------------------------------------------------------------------
(define gl-canvas%
  (class canvas%
    (inherit with-gl-context refresh swap-gl-buffers)
    
    (super-new (style '(gl)))
    
    (define initialized #f)
    
    (field (keys (make-vector 4 #f))
           (last-key #f))
    
    (define/public (leave-menu!)
      (set! in-menu #f))
    
    ;------------------------------------------------------------------------------
    ;on-size: Overrides the canvas' on-size method with a OpenGL version
    ;params: 
    ; width - the new width
    ; height - the new height
    ;------------------------------------------------------------------------------
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (gl-resize width height))))
    
    ;------------------------------------------------------------------------------
    ;on-paint: Overrides the canvas' on-paint method with a OpenGL version that encapsulates
    ;all the painting in a OpenGL context.
    ;------------------------------------------------------------------------------
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless initialized
           (gl-init) 
           (set! initialized #t))
         (gl-draw #t)
         (swap-gl-buffers))))
    
    ;------------------------------------------------------------------------------
    ;on-char: Overrides the canvas' on-char method and handles all relevant key events
    ;------------------------------------------------------------------------------
    (define/override (on-char ke)
      (if in-menu
          (unless (eq? (send ke get-key-code) 'release)
            (if in-inventory
                (case (send ke get-key-code)
                  ((up) (send (get-field inventory (get-field player world)) action 'up))
                  ((down) (send (get-field inventory (get-field player world)) action 'down))
                  ((left) (send (get-field inventory (get-field player world)) action 'left))
                  ((right) (send (get-field inventory (get-field player world)) action 'right))
                  ((escape) (set! in-inventory #f)
                            (set! in-menu #f))
                  ((#\space) (when (send (get-field inventory (get-field player world)) get-thing)
                               (send (get-field player world) interact 
                                     (send (get-field inventory (get-field player world)) get-thing))
                               (set! in-inventory #f)
                               (set! in-menu #f)))
                  ((#\i) (set! in-menu #f)
                         (set! in-inventory #f)))
                (case (send ke get-key-code)
                  ((up) (send (get-active-menu main-menu) menu-action 'up))
                  ((down) (send (get-active-menu main-menu) menu-action 'down))
                  ((#\return) (send (get-active-menu main-menu) menu-action 'enter))
                  ((#\backspace) (send (get-active-menu main-menu) menu-action 'back))
                  ((escape) (send (get-active-menu main-menu) menu-action 'back))))
            
            (set! last-key (send ke get-key-code))) 
          (if (eq? (send ke get-key-code) 'release)
              (case (send ke get-key-release-code)
                ((left) (vector-set! keys 0 #f))
                ((right) (vector-set! keys 1 #f))
                ((up) (vector-set! keys 2 #f))
                ((down) (vector-set! keys 3 #f)))
              (begin
                (case (send ke get-key-code)
                  ((left) (vector-set! keys 0 #t))
                  ((right) (vector-set! keys 1 #t))
                  ((up) (vector-set! keys 2 #t))
                  ((down) (vector-set! keys 3 #t))
                  ((escape) (set! in-menu #t)
                            (set-field! state main-menu 0)
                            (set! keys (vector #f #f #f #f)))
                  ((#\space) (unless (eq? last-key #\space)
                               (send (get-field player world) interact '())))
                  ((#\i) (set! in-menu #t)
                         (set! in-inventory #t)
                         (set! keys (vector #f #f #f #f)))
                  ((f5)   (display "Saved the game")
                          (send world savegame "Saves/quicksave.rkt"))
                  ((f9) (display "loading")
                        (send world loadgame "Saves/quicksave.rkt")
                        (display "successfully loaded the game")))
                
                (set! last-key (send ke get-key-code))))))))




;------------------------------------------------------------------------------
;gl-init: Initializes the OpenGL environment and sets up the textures.
;------------------------------------------------------------------------------
(define (gl-init)
  ;The main game timer.
  (new timer% (interval 20) (notify-callback game-tick))
  
  (glDisable GL_DEPTH_TEST)
  (include "setuptextures.rkt")
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  
  
  (glClearColor 0 0 0 1)
  (glViewport 0 0 (send glcanvas get-width) (send glcanvas get-height))
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity))

;------------------------------------------------------------------------------
;loadbackgrounds: Placeholder function to be included in the loading of tile backgrounds.
;------------------------------------------------------------------------------
(define (loadbackgrounds)
  (let ((file (open-input-file "backgrounds.cfg")))
    (define (loop)
      (let ((data (read file)))
        (unless (eof-object? data)
          (set! backgrounds (cons (cons (car data) (cadr data)) backgrounds))
          (loop))))
    (loop)))





;============================================================================
;                             Drawing and tick
;============================================================================

;------------------------------------------------------------------------------
;game-tick: The main game function that is run at every tick of the game timer.
;------------------------------------------------------------------------------
(define game-tick
  (let ((ticks 0))
    (λ ()
      (send glcanvas refresh)
      ;This will pause the game if the menu is activated.
      (unless in-menu
        (send (get-field player world) update! ticks)
        (mfor-each (λ (char)
                     (send char update! 
                           (get-field gridx (get-field player world)) 
                           (get-field gridy (get-field player world)) ticks world))
                   (let ((result '()))
                     (mfor-each (λ (char)
                                  (when (and (eqv? (get-field state world) (get-field state char))
                                             (eq? (get-field place char)
                                                  (get-field mapID (get-field current-map world))))
                                    (set! result (mcons char result))))
                                (get-field chars world))
                     result))
        
        
        (set! ticks (+ ticks 1))))))

;------------------------------------------------------------------------------
;gl-draw: The main drawing function that draws the game and calls other drawing functions in objects.
;params:
; grid? - a boolean flag. If set to #t the map will be drawn with a grid on top.
;------------------------------------------------------------------------------
(define (gl-draw dev?)
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (glOrtho (round (- (get-field xpos (get-field player world)) (/ (send glcanvas get-width) 2)) ) 
           (round (+ (get-field xpos (get-field player world)) (/ (send glcanvas get-width) 2)) )
           (round (+ (get-field ypos (get-field player world)) (/ (send glcanvas get-height) 2)))
           (round (- (get-field ypos (get-field player world)) (/ (send glcanvas get-height) 2)) )
           -1 1)
  
  ;.................
  ;      Tiles     
  ;.................
  (glEnable GL_TEXTURE_2D)
  (let* ((current-map (get-field current-map world))
         (tile-width 32)
         (map-width (get-field sizex current-map))
         (map-height (get-field sizey current-map))
         (x 0))
    (define (xloop)
      (when (< x map-width)
        (let ((y 0))
          (define (yloop)
            (when (< y map-height)
              (glMatrixMode GL_MODELVIEW)
              (glLoadIdentity)
              
              (glTranslatef (* x tile-width) (* y tile-width) 0)
              (glMatrixMode GL_PROJECTION)
              (glPushMatrix) 
              
              (glColor4f 1 1 1 1)
              
              (if (eq? (get-field type (send current-map gettile x y)) #f)
                  (glColor4f 0 0 0 1)
                  (glBindTexture 
                   GL_TEXTURE_2D 
                   (gl-vector-ref tile-texture-list 
                                  (+ (* (get-field texfamily (send current-map gettile x y)) 16)
                                     (get-field textype (send current-map gettile x y))))))
              
              (glBegin GL_TRIANGLE_STRIP)
              (glTexCoord2i 0 0)
              (glVertex2i 0 0)
              (glTexCoord2i 1 0)
              (glVertex2i tile-width 0)
              (glTexCoord2i 0 1)
              (glVertex2i 0 tile-width)
              (glTexCoord2i 1 1)
              (glVertex2i tile-width tile-width)
              (glEnd)
              (when dev?
                (glColor4f 1 1 1 0.2)
                (glDisable GL_TEXTURE_2D)
                (glBegin GL_LINES)
                (glVertex2i 0 0)
                (glVertex2i tile-width 0)
                (glEnd)
                (glBegin GL_LINES)
                (glVertex2i tile-width 0)
                (glVertex2i tile-width tile-width)
                (glEnd)
                (glBegin GL_LINES)
                (glVertex2i tile-width tile-width)
                (glVertex2i 0 tile-width)
                (glEnd)
                (glBegin GL_LINES)
                (glVertex2i 0 tile-width)
                (glVertex2i 0 0)
                (glEnd)
                (glEnable GL_TEXTURE_2D))
              (glPopMatrix)
              (set! y (+ 1 y))
              (yloop)))
          (yloop)
          (set! x (+ 1 x))
          (xloop))))
    (xloop))
  
  
  ;................
  ; Characters    
  ;................
  
  (mfor-each (lambda (agent)
               
               (glMatrixMode GL_MODELVIEW)
               (glLoadIdentity)
               (glTranslatef (get-field xpos agent) (get-field ypos agent) 0)
               (glMatrixMode GL_PROJECTION)
               (glPushMatrix)
               
               (glBindTexture GL_TEXTURE_2D (gl-vector-ref char-texture-list (get-field tex-ID agent)))
               (glColor3f 1 1 1)
               (glBegin GL_TRIANGLE_STRIP)
               (glTexCoord2i 0 0)
               (glVertex2i 0 0)
               (glTexCoord2i 1 0)
               (glVertex2i 32 0)
               (glTexCoord2i 0 1)
               (glVertex2i 0 32)
               (glTexCoord2i 1 1)
               (glVertex2i 32 32)
               (glEnd)
               (glPopMatrix))
             (let ((result '()))
               (mfor-each (λ (character)
                            (when (and (= (get-field state character)
                                          (get-field state world))
                                       (eqv? (get-field place character)
                                             (get-field mapID (get-field current-map world))))
                              (set! result (mcons character result))))
                          (get-field chars world))
               result))
  
  
  ;................
  ;      Things   
  ;................
  
  (mfor-each (lambda (thing)
               (glMatrixMode GL_MODELVIEW)
               (glLoadIdentity)
               (glTranslatef (get-field xpos thing) (get-field ypos thing) 0)
               (glMatrixMode GL_PROJECTION)
               (glPushMatrix)
               (glBindTexture GL_TEXTURE_2D (gl-vector-ref char-texture-list (get-field tex-ID thing)))
               
               (glColor3f 1 1 1)
               (glBegin GL_TRIANGLE_STRIP)
               (glVertex2i 0 0 )
               (glVertex2i 0 32)
               (glVertex2i 32 0)
               (glVertex2i 32 32)
               (glEnd)
               (glPopMatrix))
             (let ((result '()))
               (mfor-each (λ (thing)
                            (when (eqv? (get-field place thing)
                                        (get-field mapID (get-field current-map world)))
                              (set! result (mcons thing result))))
                          (get-field things world))
               result))
  
  
  ;..............
  ;     Player  
  ;..............
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glTranslatef (get-field xpos (get-field player world)) (get-field ypos (get-field player world)) 0) 
  (glMatrixMode GL_PROJECTION)
  (glPushMatrix)
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref char-texture-list 0))
  (glColor3f 1 1 1)
  (glBegin GL_TRIANGLE_STRIP)
  (glTexCoord2i 0 0)
  (glVertex2i 0 -32)
  (glTexCoord2i 1 0)
  (glVertex2i 32 -32)
  (glTexCoord2i 0 1)
  (glVertex2i 0 32)
  (glTexCoord2i 1 1)
  (glVertex2i 32 32)
  (glEnd)
  
  ;---------------------------
  ; draw-message
  ;--------------------------
  (unless (null? (unbox message-list-box))
          (draw-text (list-ref (mcar (unbox message-list-box)) 0)
                     (list-ref (mcar (unbox message-list-box)) 1)
                     (list-ref (mcar (unbox message-list-box)) 2)
                     (list-ref (mcar (unbox message-list-box)) 3)
                     text-texture-list)
          (set-box! message-list-box (mcdr (unbox message-list-box))))
  
  ;.............
  ;      Mask  
  ; The overlay causing the "field-of-vision effect"
  ;.............
  (glMatrixMode GL_MODELVIEW)
  (glTranslatef 16 0 0)
  (glPushMatrix)
  (glRotatef 
   (get-field angle (get-field player world))
   0 0 1)
  (glMatrixMode GL_PROJECTION)
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 1))
  (glColor4f 1 1 1 0.95)
  (glBegin GL_TRIANGLE_STRIP)
  (glTexCoord2i 0 0)
  (glVertex2i -300 -600)
  (glTexCoord2i 1 0)
  (glVertex2i 300 -600)
  (glTexCoord2i 0 1)
  (glVertex2i -300 100)
  (glTexCoord2i 1 1)
  (glVertex2i 300 100)
  (glEnd)
  
  (glDisable GL_TEXTURE_2D)
  (glBegin GL_TRIANGLE_STRIP)
  (glColor4f 0 0 0 1)
  (glVertex2i -1000 -1000)
  (glVertex2i 1000 -1000)
  
  (glColor4f 0 0 0 0.95)
  (glVertex2i -300 -600)
  (glVertex2i 300 -600)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glColor4f 0 0 0 1)
  (glVertex2i 1000 -1000)
  (glVertex2i 1000 1000)
  
  (glColor4f 0 0 0 0.95)
  (glVertex2i 300 -600)
  (glVertex2i 300 100)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glColor4f 0 0 0 1)
  (glVertex2i 1000 1000)
  (glVertex2i -1000 1000)
  
  (glColor4f 0 0 0 0.95)
  (glVertex2i 300 100)
  (glVertex2i -300 100)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glColor4f 0 0 0 1)
  (glVertex2i -1000 1000)
  (glVertex2i -1000 -1000)
  
  (glColor4f 0 0 0 0.95)
  (glVertex2i -300 100)
  (glVertex2i -300 -600)
  (glEnd)
  (glColor4f 1 1 1 1)
  
  (glPopMatrix)
  (when dev?
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glTranslatef (+ (- (get-field xpos (get-field player world)) (/ (send glcanvas get-width) 2)) 10)
                  (+ (- (get-field ypos (get-field player world)) (/ (send glcanvas get-height) 2)) 10)
                  0)
    (glMatrixMode GL_PROJECTION)
    
    (draw-text 0 0
               0.5  
               (string-append "Current-pos - " (number->string (get-field gridx (get-field player world))) ", " (number->string (get-field gridy (get-field player world))))
               text-texture-list)
    (draw-text 0 20
               0.5
               (string-append "World state - " (number->string (get-field state world)))
               text-texture-list)
    (draw-text 0 40
               0.5
               (string-append "Map - " (symbol->string (get-field mapID (get-field current-map world))))
               text-texture-list))
  ;---------------
  ;       Menu   
  ;---------------
  
  (when in-menu
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glTranslatef (- (get-field xpos (get-field player world)) (/ (send glcanvas get-width) 2)) 
                  (- (get-field ypos (get-field player world)) (/ (send glcanvas get-height) 2)) 
                  0)
    (glMatrixMode GL_PROJECTION)
    
    (glColor4f 0 0 0 0.7)
    (glBegin GL_TRIANGLE_STRIP)
    (glVertex2f 0 0)
    (glVertex2f (send glcanvas get-width) 0)
    (glVertex2f 0 (send glcanvas get-height))
    (glVertex2f (send glcanvas get-width) (send glcanvas get-height))
    (glEnd)
    (glTranslatef 200 50 0)
    (cond
      (in-inventory (send (get-field inventory (get-field player world)) draw text-texture-list))
      (in-interactions-menu (send interactions-menu render interactions-menu text-texture-list))
      (else
       (send main-menu render main-menu text-texture-list)))
    
    (glPopMatrix)))



;------------------------------------------------------------------------------
;gl-resize: Simply resizes the OpenGL viewport.
;params: 
; width - the new width
; height - the new height
;------------------------------------------------------------------------------
(define (gl-resize width height)
  (glViewport 0 0 width height)
  (send glcanvas refresh))

;------------------------------------------------------------------------------
;game-init: Initializes the game environment.
;------------------------------------------------------------------------------
(define (game-init)
  (set-field! parent main-menu glcanvas)
  (send world character-load (dynamic-require "Gamedata/Agentdata.rkt" 'Character-list))
  (send world add-things! (Load-things (dynamic-require "Gamedata/Agentdata.rkt" 'Thing-list) world))
  (send world add-map! (load&create-map 'Workroom "maps/Workroom.stuff" world))
  (send world set-current-map! 'first))


;============================================================================
;                           Object declarations
;============================================================================

(define frame (new frame% 
                   (width 800) 
                   (height 600) 
                   (label "Project Terralay")))


(define glcanvas (new gl-canvas% 
                      (parent frame)))


(define world (new World%
                   (maplist '())
                   (current-map #f)
                   (canvas glcanvas)
                   (state 0)
                   (message-list-box message-list-box)))

(define main-menu ((dynamic-require "setupmenus.rkt" 'setup-main-menu) world))


(define interactions-menu (new Menu%
                               (title "text")
                               (world world)
                               (parent glcanvas)
                               (button-functions '())
                               (children '())))


(game-init)
;Start it up

(send frame show #t)

