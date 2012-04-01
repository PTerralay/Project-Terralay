#lang racket/gui
(require sgl sgl/gl sgl/gl-vectors)
(provide gl-init gl-draw glcanvas frame)

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
         (swap-gl-buffers))))))
    
(define (gl-init)
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_TEXTURE_2D)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  (glClearColor 1 1 1 1)
  (glViewport 0 0 (send glcanvas get-width) (send glcanvas get-height))
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  )


(define (gl-draw)
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (glPushMatrix)
  (glOrtho 0 (send glcanvas get-width) (send glcanvas get-height) 0 -1 1)
  
  
  
  
  (glPopMatrix))

(define frame (new frame% 
                   (width 800) 
                   (height 600) 
                   (label "Project Terralay")))
(define glcanvas (new gl-canvas% (parent frame)))