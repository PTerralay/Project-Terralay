;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;

#lang racket/gui


(require "world.rkt" "Player.rkt" "Map.rkt" "Tile.rkt"
         plot/utils
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
              ((down) (vector-set! keys 3 #t)))
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
  (set! texture-list (glGenTextures 11))
  (glEnable GL_TEXTURE_2D)
  (define floortex (image->gl-vector "images/floortile.png"))
  (define walltexleft (image->gl-vector "images/walltileleft.png"))
  (define walltexright (image->gl-vector "images/walltileright.png"))
  (define walltextop (image->gl-vector "images/walltiletop.png"))
  (define walltexbot (image->gl-vector "images/walltilebottom.png"))
  (define walltexspecleft (image->gl-vector "images/specwallleft.png"))
  (define walltexspecright (image->gl-vector "images/specwallright.png"))
  (define walltexcornerbotl (image->gl-vector "images/wallcornerbotleft.png"))
  (define walltexcornerbotr (image->gl-vector "images/wallcornerbotright.png"))
  (define playertex (image->gl-vector "images/player.png"))
  (define mask (image->gl-vector "images/mask.png"))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 0))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref floortex 0) (list-ref floortex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref floortex 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 1))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexleft 0) (list-ref walltexleft 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexleft 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 2))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexright 0) (list-ref walltexright 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexright 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 3))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltextop 0) (list-ref walltextop 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltextop 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 4))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexbot 0) (list-ref walltexbot 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexbot 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 5))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexspecleft 0) (list-ref walltexspecleft 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexspecleft 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 6))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexspecright 0) (list-ref walltexspecright 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexspecright 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 7))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexcornerbotl 0) (list-ref walltexcornerbotl 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexcornerbotl 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 8))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexcornerbotr 0) (list-ref walltexcornerbotr 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexcornerbotr 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 9))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref playertex 0) (list-ref playertex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref playertex 2))
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 10))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref mask 0) (list-ref mask 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref mask 2))
  
  
  
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


;_________________________________________________________________________
;              Temporary image loading function, needs alpha handling
;-------------------------------------------------------------------------
(define (bitmap->gl-vector bmp)
  (let* (
         (dc (instantiate bitmap-dc% (bmp)))
         (pixels (* (send bmp get-width) (send bmp get-height)))
         (vec (make-gl-ubyte-vector (* pixels 4)))
         (data (make-bytes (* pixels 4)))
         (i 0)
         )
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
  (glOrtho (round (- (send player get-xpos) (/ (send glcanvas get-width) 2)) ) 
           (round (+ (send player get-xpos) (/ (send glcanvas get-width) 2)) )
           (round (+ (send player get-ypos) (/ (send glcanvas get-height) 2)))
           (round (- (send player get-ypos) (/ (send glcanvas get-height) 2)) )
           -1 1)
  (let ((current-map (send world get-current-map))
        (tile-width 32)
        (x 0))
    (define (xloop)
      (when (< x (send current-map get-sizex))
        (let ((y 0))
          (define (yloop)
            (when (< y (send current-map get-sizey))
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
              (glTranslatef 0 0 0)
              (set! y (+ 1 y))
              (yloop)))
          (yloop)
          (set! x (+ 1 x))
          (xloop))))
    (xloop))
  
  
  
  
  
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glTranslatef (send player get-xpos) (send player get-ypos) 0)
  
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
  
  
  
  (glMatrixMode GL_MODELVIEW)
  (glTranslatef 16 0 0)
  (glRotatef 
   (case (send player get-dir)
     ((up) 0)
     ((left) 270)
     ((right) 90)
     ((down) 180))
   0 0 1)
  (glMatrixMode GL_PROJECTION)
  
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 10))'
  (glColor4f 1 1 1 1)
  (glBegin GL_TRIANGLE_STRIP)
  (glTexCoord2i 0 0)
  (glVertex2i -500 -800)
  (glTexCoord2i 1 0)
  (glVertex2i 500 -800)
  (glTexCoord2i 0 1)
  (glVertex2i -500 200)
  (glTexCoord2i 1 1)
  (glVertex2i 500 200)
  (glEnd)
  
  (glDisable GL_TEXTURE_2D)
  (glColor4f 0 0 0 1)
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i -498 -1000)
  (glVertex2i -498 1000)
  (glVertex2i -1000 -1000)
  (glVertex2i -1000 1000)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i -1000 198)
  (glVertex2i 1000 198)
  (glVertex2i -1000 1000)
  (glVertex2i 1000 1000)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i 498 -1000)
  (glVertex2i 498 1000)
  (glVertex2i 1000 -1000)
  (glVertex2i 1000 1000)
  (glEnd)
  
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i -1000 -798)
  (glVertex2i 1000 -798)
  (glVertex2i -1000 -1000)
  (glVertex2i 1000 -1000)
  (glEnd)
  
  
  
  
  (glEnable GL_TEXTURE_2D)
  
;  (glBegin GL_TRIANGLES)
;  (let* ((fov 60) ;Must be > 90!
;         (half-diag 1000)
;         (delta-x (abs (* 1.5 half-diag (sin (degrees->radians (/ fov 2))))))
;         (delta-y (abs (* 1.5 half-diag (cos (degrees->radians (/ fov 2)))))))
;    
;    (newline)
;    (display delta-x)
;    (newline)
;    (display delta-y)
;    (newline)
;    (newline)
;    
;   
;    (glColor4f 0 0 0 0.85)
;    (glVertex2f 0 128)
;    (glVertex2f (- half-diag) (- 128 half-diag))
;    (glVertex2f (- half-diag) (+ 128 half-diag))
;    
;    (glVertex2f 0 128)
;    (glVertex2f (- half-diag) (+ 128 half-diag))
;    (glVertex2f half-diag (+ 128 half-diag))
;    
;    (glVertex2f 0 128)
;    (glVertex2f half-diag (+ 128 half-diag))
;    (glVertex2f half-diag (- 128 half-diag))
;    
;    
;    (glVertex2f 0 128)
;    (glVertex2f (- delta-x) (- 128 delta-y))
;    (glVertex2f (- half-diag) (- 128 half-diag))
;    
;    
;    (glVertex2f 0 128)
;    (glVertex2f delta-x (- 128 delta-y))
;    (glVertex2f half-diag (- 128 half-diag))
;    
;    (glEnd)
;    
;    (glBegin GL_TRIANGLE_STRIP)
;    (glColor4f 0 0 0 0)
;    (glVertex2f (- delta-x) (- delta-y))
;    (glVertex2f 0 0)
;    (glColor4f 0 0 0 0.85)
;    (glVertex2f (- delta-x) (- 128 delta-y))
;    (glVertex2f 0 128)
;    (glEnd)
;    
;    (glBegin GL_TRIANGLE_STRIP)
;    (glColor4f 0 0 0 0)
;    (glVertex2f delta-x (- delta-y))
;    (glVertex2f 0 0)
;    (glColor4f 0 0 0 0.85)
;    (glVertex2f delta-x (- 128 delta-y))
;    (glVertex2f 0 128)
;    (glEnd)
;    )
  
  
  
  
  (glPopMatrix)
  
  (glPopMatrix))

(define (gl-resize width height)
  (glViewport 0 0 width height)
  (send glcanvas refresh))
  ;(send glcanvas update-diag (sqrt (+ (* (send glcanvas get-width) (send glcanvas get-width)) (* (send glcanvas get-height) (send glcanvas get-height))))))


(define game-tick
  (let ((ticks 0))
    (lambda ()
      (send glcanvas refresh)
      (send player move-update!))))

;----------------------------------------------------------------------------
;                           Object declarations
;----------------------------------------------------------------------------

(define backgrounds '())
(define texture-list #f)
(define mapplista (list (Load&Create 'test-room "Awesomeroom.txt")))

(define world (new World%
                   (maplist mapplista) 
                   (current-map (car mapplista)) 
                   (state 0)))

(define frame (new frame% 
                   (width 800) 
                   (height 600) 
                   (label "Project Terralay")))

(define glcanvas (new gl-canvas% 
                      (parent frame)))

(define *world* (new World%
                     (maplist mapplista) 
                     (current-map (car mapplista))
                     (state 0)))

(define player (instantiate Player% (32 32 1 1 'up *world* glcanvas)))


;Start it up

(send frame show #t)

(define (printtypes map)
  (let ((tiles (send map get-tile-vector)))
    (for-each (lambda (thavector)
                (for-each (lambda (tile)
                            (display (send tile get-type))) 
                          (vector->list thavector))
                (newline))
              (vector->list tiles))))