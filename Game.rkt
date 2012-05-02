;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;

#lang racket/gui

(require "World.rkt" "Player.rkt" "Map.rkt" "Thing.rkt" "Menu.rkt" "drawtext.rkt" "graphics-utils.rkt"
         racket/mpair
         sgl/gl
         sgl/gl-vectors)


;----------------------------------
;    Module Variables
;----------------------------------
(define backgrounds '())
(define texture-list #f)
(define in-menu #f)
(define in-inventory #f)


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
         (gl-draw #f)
         (swap-gl-buffers))))
    
    ;------------------------------------------------------------------------------
    ;on-char: Overrides the canvas' on-char method and handles all relevant key events
    ;------------------------------------------------------------------------------
    (define/override (on-char ke)
      (if in-menu
          (unless (eq? (send ke get-key-code) 'release)
            (if in-inventory
                (case (send ke get-key-code)
                  ((up) (send (send (send world get-player) get-inventory) action 'up))
                  ((down) (send (send (send world get-player) get-inventory) action 'down))
                  ((left) (send (send (send world get-player) get-inventory) action 'left))
                  ((right) (send (send (send world get-player) get-inventory) action 'right))
                  ((escape) (set! in-inventory #f)
                            (set! in-menu #f))
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
                  ((#\space) (when (not (eq? last-key #\space))
                               (send (send world get-player) interact )))
                  ((#\i) (set! in-menu #t)
                         (set! in-inventory #t)
                         (set! keys (vector #f #f #f #f))))
                
                (set! last-key (send ke get-key-code))))))))


(define (loop parent)
  (for-each (λ (menu)
              (display (get-field title menu)) (display ": ") (display (get-field state menu)) (newline)
              (loop menu))
            (get-field children parent)))

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
      (when main-menu
        (loop main-menu))
      (send glcanvas refresh)
      ;This will pause the game if the menu is activated.
      (unless in-menu
        (send (send world get-player) update! ticks)
        (mfor-each (λ (char)
                     (send char update! 
                           (send (send world get-player) getx) 
                           (send (send world get-player) gety) ticks world))
                   (let ((result '()))
                     (mfor-each (λ (char)
                                  (when (eq? (send char getplace)
                                             (get-field mapID (send world get-current-map)))
                                    (set! result (mcons char result))))
                                (send world get-chars))
                     result))
        (set! ticks (+ ticks 1))))))

;------------------------------------------------------------------------------
;gl-draw: The main drawing function that draws the game and calls other drawing functions in objects.
;params:
; grid? - a boolean flag. If set to #t the map will be drawn with a grid on top.
;------------------------------------------------------------------------------
(define (gl-draw grid?)
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (glOrtho (round (- (send (send world get-player) get-xpos) (/ (send glcanvas get-width) 2)) ) 
           (round (+ (send (send world get-player) get-xpos) (/ (send glcanvas get-width) 2)) )
           (round (+ (send (send world get-player) get-ypos) (/ (send glcanvas get-height) 2)))
           (round (- (send (send world get-player) get-ypos) (/ (send glcanvas get-height) 2)) )
           -1 1)
  
  ;.................
  ;      Tiles     
  ;.................
  (glEnable GL_TEXTURE_2D)
  (let* ((current-map (send world get-current-map))
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
              (case (send (send current-map gettile x y) get-type)
                
                ((#\space) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 0)))
                ((#\l) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 1)))
                ((#\r) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 2)))
                ((#\t) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 3)))
                ((#\b) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 4)))
                ((#\1) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 5)))
                ((#\2) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 6)))
                ((#\3) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 7)))
                ((#\4) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 8)))
                ((#\h #\d) (glColor3f 0 0 0)))
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
              (when grid?
                (glColor3f 0 0 0)
                (glBegin GL_LINES)
                (glVertex2i 0 0)
                (glVertex2i tile-width 0)
                (glVertex2i tile-width tile-width)
                (glVertex2i 0 tile-width)
                (glEnd))
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
               (glTranslatef (send agent get-xpos) (send agent get-ypos) 0)
               (glMatrixMode GL_PROJECTION)
               (glPushMatrix)
               (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 11))
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
                            (when (eqv? (send character getplace)
                                        (get-field mapID (send world get-current-map)))
                              (set! result (mcons character result))))
                          (send world get-chars))
               result))
  
  (glDisable GL_TEXTURE_2D)
  
  ;................
  ;      Things   
  ;................
  
  (mfor-each (lambda (thing)
               (glMatrixMode GL_MODELVIEW)
               (glLoadIdentity)
               (glTranslatef (send thing get-xpos) (send thing get-ypos) 0)
               (glMatrixMode GL_PROJECTION)
               (glPushMatrix)
               (glColor3f 1 0 0)
               (glBegin GL_TRIANGLE_STRIP)
               (glVertex2i 0 0 )
               (glVertex2i 0 32)
               (glVertex2i 32 0)
               (glVertex2i 32 32)
               (glEnd)
               (glPopMatrix))
             (let ((result '()))
               (mfor-each (λ (thing)
                            (when (eqv? (send thing get-place)
                                        (get-field mapID (send world get-current-map)))
                              (set! result (mcons thing result))))
                          (send world get-things))
               result))
  
  (glEnable GL_TEXTURE_2D)
  
  ;..............
  ;     Player  
  ;..............
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glTranslatef (send (send world get-player) get-xpos) (send (send world get-player) get-ypos) 0) 
  (glMatrixMode GL_PROJECTION)
  (glPushMatrix)
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 9))
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
  
  
  ;.............
  ;      Mask  
  ; The overlay causing the "field-of-vision effect"
  ;.............
  (glMatrixMode GL_MODELVIEW)
  (glTranslatef 16 0 0)
  (glPushMatrix)
  (glRotatef 
   (send (send world get-player) get-angle)
   0 0 1)
  (glMatrixMode GL_PROJECTION)
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 10))
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
  
  ;---------------
  ;       Menu   
  ;---------------
  
  (when in-menu
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glTranslatef (- (send (send world get-player) get-xpos) (/ (send glcanvas get-width) 2)) (- (send (send world get-player) get-ypos) (/ (send glcanvas get-height) 2)) 0)
    (glMatrixMode GL_PROJECTION)
    
    (glColor4f 0 0 0 0.7)
    (glBegin GL_TRIANGLE_STRIP)
    (glVertex2f 0 0)
    (glVertex2f (send glcanvas get-width) 0)
    (glVertex2f 0 (send glcanvas get-height))
    (glVertex2f (send glcanvas get-width) (send glcanvas get-height))
    (glEnd)
    (glTranslatef 200 50 0)
    
    (if in-inventory
        (begin
          (send (send (send world get-player) get-inventory) draw texture-list))
        (begin
          (send main-menu render main-menu texture-list)))
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
  (send world add-map! (load&create-map 'Awesomeroom "maps/Awesomeroom.stuff" world))
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

(define main-menu ((dynamic-require "setupmenus.rkt" 'setup-main-menu)))


(define world (new World%
                   (maplist '())
                   (current-map #f)
                   (canvas glcanvas)
                   (state 0)))

(game-init)
;Start it up

(send frame show #t)

