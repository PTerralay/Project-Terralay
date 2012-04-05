;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;

#lang racket/gui

(require "World.rkt" "Player.rkt" "Map.rkt" "Thing.rkt" "Menu.rkt"
         racket/mpair
         sgl/gl
         sgl/gl-vectors)

;--------------------------------------------------------------------------------
;                                  Init
;--------------------------------------------------------------------------------
(define gl-canvas%
  (class canvas%
    (inherit with-gl-context refresh swap-gl-buffers)
    
    (super-new (style '(gl)))
    
    (define initialized #f)
    
    
    
    (define keys (make-vector 4 #f))
    (define last-key #f)
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless initialized
           (gl-init)
           (set! initialized #t))
         (gl-draw #f)
         (swap-gl-buffers))))
    
    (define/override (on-char ke)
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
              ((#\space) (when (not (eq? last-key #\space))
                                    (send (send world get-player) interact )))
              ((#\i) (show-inventory (send world get-player))))
            (set! last-key (send ke get-key-code)))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (gl-resize width height))))
    
    (define/public (get-keys)
      keys)
    (define/public (get-last-key)
      last-key)))


(define (gl-init)
  (new timer% (interval 20) (notify-callback game-tick))
  
  (glDisable GL_DEPTH_TEST)
  
  (include "setuptextures.rkt")
  
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  (glClearColor 0 0 0 1)
  (glViewport 0 0 (send glcanvas get-width) (send glcanvas get-height))
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity))

(define (loadbackgrounds)
  (let ((file (open-input-file "backgrounds.cfg")))
    (define (loop)
      (let ((data (read file)))
        (unless (eof-object? data)
          (set! backgrounds (cons (cons (car data) (cadr data)) backgrounds))
          (loop))))
    (loop)))


;__________________________________________________________________________________
;              Image loading function, based on example code bundled with drracket
;----------------------------------------------------------------------------------
(define (bitmap->gl-vector bmp)
  (let* ((dc (instantiate bitmap-dc% (bmp)))
         (pixels (* (send bmp get-width) (send bmp get-height)))
         (vec (make-gl-ubyte-vector (* pixels 4)))
         (data (make-bytes (* pixels 4)))
         (i 0))
    (send dc get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) data)
    (letrec
        ([loop
          (lambda ()
            (when (< i pixels)
              (begin
                (gl-vector-set! vec (* i  4) 
                                (bytes-ref data (+ (* i 4) 1)))
                (gl-vector-set! vec (+ (* i 4) 1) 
                                (bytes-ref data (+ (* i 4) 2)))
                (gl-vector-set! vec (+ (* i 4) 2) 
                                (bytes-ref data (+ (* i 4) 3)))
                (gl-vector-set! vec (+ (* i 4) 3) 
                                (bytes-ref data (+ (* i 4) 0)))
                
                (set! i (+ i 1))
                (loop))))])
      (loop))
    (send dc set-bitmap #f)
    (list (send bmp get-width) (send bmp get-height) vec)))

(define (image->gl-vector file) (bitmap->gl-vector (make-object bitmap% file 'png/alpha #f)))



;-----------------------------------------------------------------------------
;                           Drawing and Tick
;-----------------------------------------------------------------------------

(define (gl-draw grid?)
  
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (glPushMatrix)
  (glOrtho (round (- (send (send world get-player) get-xpos) (/ (send glcanvas get-width) 2)) ) 
           (round (+ (send (send world get-player) get-xpos) (/ (send glcanvas get-width) 2)) )
           (round (+ (send (send world get-player) get-ypos) (/ (send glcanvas get-height) 2)))
           (round (- (send (send world get-player) get-ypos) (/ (send glcanvas get-height) 2)) )
           -1 1)
  
  ;.........................
  ; Tiles
  ;.........................
  (let* ((current-map (send world get-current-map))
         (tile-width 32)
         (map-width (send current-map get-sizex))
         (map-height (send current-map get-sizey))
         (x 0))
    (define (xloop)
      (when (< x map-width)
        (let ((y 0))
          (define (yloop)
            (when (< y map-height)
              (glMatrixMode GL_MODELVIEW)
              (glLoadIdentity)
              (if grid?
                  (glTranslatef (* x (+ 1 tile-width))  (* y (+ 1 tile-width)) 0)
                  (glTranslatef (* x tile-width) (* y tile-width) 0))
              (glMatrixMode GL_PROJECTION)
              (glPushMatrix) 
              
              (glColor4f 1 1 1 1)
              (case (send (send current-map gettile x y) get-type)
                
                ((#\f) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 0)))
                ((#\l) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 1)))
                ((#\r) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 2)))
                ((#\t) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 3)))
                ((#\b) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 4)))
                ((#\1) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 5)))
                ((#\2) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 6)))
                ((#\3) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 7)))
                ((#\4) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 8)))
                ((#\h) (glColor3f 0 0 0)))
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
              (glPopMatrix)
              (set! y (+ 1 y))
              (yloop)))
          (yloop)
          (set! x (+ 1 x))
          (xloop))))
    (xloop))
  
  
  ;.........................
  ; Characters
  ;.........................
  
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
             (send (send world get-current-map) get-characters))
  
  (glDisable GL_TEXTURE_2D)
  
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
             (send (send world get-current-map) get-things))
  (glEnable GL_TEXTURE_2D)
                           
  
  ;.........................
  ; player
  ;.........................
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
  
  
  ;.........................
  ; Mask
  ;.........................
  (glMatrixMode GL_MODELVIEW)
  (glTranslatef 16 0 0)
  
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
  (glColor4f 0 0 0 0.95)
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i -300 -1000)
  (glVertex2i -300 1000)
  (glVertex2i -1000 -1000)
  (glVertex2i -1000 1000)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i -1000 100)
  (glVertex2i 1000 100)
  (glVertex2i -1000 1000)
  (glVertex2i 1000 1000)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i 300 -1000)
  (glVertex2i 300 1000)
  (glVertex2i 1000 -1000)
  (glVertex2i 1000 1000)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i -1000 -600)
  (glVertex2i 1000 -600)
  (glVertex2i -1000 -1000)
  (glVertex2i 1000 -1000)
  (glEnd)
  
  (glEnable GL_TEXTURE_2D)
  
  (glPopMatrix)
  
  (glPopMatrix))

(define (gl-resize width height)
  (glViewport 0 0 width height)
  (send glcanvas refresh))

(define game-tick
  (let ((ticks 0))
    (lambda ()
      (send glcanvas refresh)
      (send (send world get-player) update! ticks)
      (mfor-each (lambda (agent)
                   (send agent update! 
                         (send (send world get-player) getx) 
                         (send (send world get-player) gety) ticks world))
                 (send (send world get-current-map) get-characters))
      (set! ticks (+ ticks 1)))))

;----------------------------------------------------------------------------
;                           Object declarations
;----------------------------------------------------------------------------

(define backgrounds '())
(define texture-list #f)





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
                   (state 0)))

(send world add-map! (load&create-map 'test-room "maps/Awesomeroom.stuff" world))
(send world set-current-map! 'first)

;(define Tetsy (LoadChar "testmonster.txt" world))


;Start it up

(send frame show #t)

