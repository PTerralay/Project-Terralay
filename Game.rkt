;---------------------------------------------------------;
;                                                         ;
; Game.rkt - Main game file for Project Terralay          ;
; Authors: Karl Linderhed and Oskar Jansson               ;
;                                                         ;
;---------------------------------------------------------;
#lang racket/gui


(require "world.rkt" "player.rkt" "map.rkt" "tile.rkt"
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
         (gl-resize))))))

(define (gl-init)
  (new timer% (interval 500) (notify-callback tick))
  
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_TEXTURE_2D)
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
              ;(glTranslatef (* x tile-width) (* y tile-width) 0) ;Fix the following!!!
              (glPushMatrix)
              (glBegin GL_TRIANGLE_STRIP)
              (case (send (send current-map gettile x y) get-type)
                ((#\w) (glColor3f 1 0 0))
                ((#\f) (glColor3f 0 1 0))
                ((#\h) (glColor3f 0 0 1)))
              (glVertex2i (* x 64) (* y 64))
              (glVertex2i (+ (* x 64) tile-width) (* y 64))
              (glVertex2i (* x 64) (+ (* y 64) tile-width))
              (glVertex2i (+ (* x 64) tile-width) (+ (* y 64) tile-width))
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