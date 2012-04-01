;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;
#lang racket/gui


(require "world.rkt" "Player.rkt" "Map.rkt" "Tile.rkt"
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
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless initialized
           (gl-init)
           (set! initialized #t))
         (gl-draw)
         (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (gl-resize width height))))))

(define (gl-init)
  (new timer% (interval 500) (notify-callback tick))
  
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_TEXTURE_2D)
  (define walltex (image->gl-vector "wall.png"))
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 0))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  
  (glTexImage2D GL_TEXTURE_2D 0 3 (list-ref walltex 0) (list-ref walltex 1) 0 GL_RGB GL_UNSIGNED_BYTE (list-ref walltex 2))
  
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  (glClearColor 1 1 1 1)
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
         (vec (make-gl-ubyte-vector (* pixels 3)))
         (data (make-bytes (* pixels 4)))
         (i 0)
         )
    (send dc get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) data)
    (letrec
        ([loop
          (lambda ()
            (when (< i pixels)
              (begin
                (gl-vector-set! vec (* i  3) 
                                (bytes-ref data (+ (* i 4) 1)))
                (gl-vector-set! vec (+ (* i 3) 1) 
                                (bytes-ref data (+ (* i 4) 2)))
                (gl-vector-set! vec (+ (* i 3) 2) 
                                (bytes-ref data (+ (* i 4) 3)))
                
                (set! i (+ i 1))
                (loop))))])
      (loop))
    (send dc set-bitmap #f)
    (list (send bmp get-width) (send bmp get-height) vec)))

(define (image->gl-vector file) (bitmap->gl-vector (make-object bitmap% file 'unknown #f)))



;-----------------------------------------------------------------------------
;                           Drawing and Tick
;-----------------------------------------------------------------------------

(define (gl-draw)
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (glPushMatrix)
  (glOrtho 0 (send glcanvas get-width) (send glcanvas get-height) 0 -1 1)
  
  (let ((current-map (send world get-current-map))
        (tile-width 64)
        (x 0))
    (define (xloop)
      (when (< x (send current-map get-sizex))
        (let ((y 0))
          (define (yloop) 
            (when (< y (send current-map get-sizey))
              (glMatrixMode GL_MODELVIEW)
              (glLoadIdentity)
              (glTranslatef (* x tile-width) (* y tile-width) 0)
              (glMatrixMode GL_PROJECTION)
              (glPushMatrix)
              (glBegin GL_TRIANGLE_STRIP)
              (case (send (send current-map gettile x y) get-type)
                ((#\w) (glColor3f 1 0 0))
                ((#\f) (glColor3f 0 1 0))
                ((#\h) (glColor3f 0 0 1)))
              (glVertex2i 0 0)
              (glVertex2i tile-width 0)
              (glVertex2i 0 tile-width)
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
  
  (glPopMatrix))

(define (gl-resize width height)
  (glViewport 0 0 width height)
  (send glcanvas refresh))



(define (tick)
  (send glcanvas refresh))

;----------------------------------------------------------------------------
;                           Object declarations
;----------------------------------------------------------------------------

(define backgrounds '())

(define mapplista (list (Load&Create 'test-room "Loadtest.txt")))
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