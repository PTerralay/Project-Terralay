#lang racket/gui
(require sgl/gl sgl/gl-vectors "3dobject.rkt" "graphics-utils.rkt")

(define rotate 0)

(define player%
  (class object%
    (super-new)
    (field (x 0)
           (y 0))))


(define player (new player%))
(define grid (vector (vector 1 1 1 1)
                     (vector 1 0 0 1)
                     (vector 1 1 0 1)
                     (vector 1 1 1 1)))



(define proftex (image->gl-vector "images/prof2.png"))
(define texlist (glGenTextures 1))
(define glcanvas%
  (class canvas%
    (inherit with-gl-context refresh swap-gl-buffers)
    (field (initialized #f))
    (super-new (style '(gl)))
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless initialized
           (gl-init)
           (set! initialized #t))
         (gl-draw)
         (swap-gl-buffers))))
    
    (define/override (on-char ke)
      (let ((kc (send ke get-key-code)))
        (if (eq? kc 'release)
            (void)
            (case kc
              ((left) (set-field! x player (- (get-field x player) 1)))
              ((right) (set-field! x player (+ (get-field x player) 1)))))))))

(define (gl-init)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_BLEND)
  (glEnable GL_TEXTURE_2D)
  
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref texlist 0))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref proftex 0) (list-ref proftex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref proftex 2))
  
  
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  
  
  (glClearColor 0 0 0 1)
  (glViewport 0 0 (send glcanvas get-width) (send glcanvas get-height))
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 45 (/ (send glcanvas get-width) (send glcanvas get-height))  10 1000)
  
  (glMatrixMode GL_MODELVIEW)
  (new timer% (interval 5
                        ) (notify-callback tick)))

(define gl-draw
  
  (lambda ()
    (glClear GL_COLOR_BUFFER_BIT)
    (glClear GL_DEPTH_BUFFER_BIT)
    (glLoadIdentity)
    (gluLookAt (get-field x player) (+ (get-field y player) 0) 10 0 0 0 0 0 1)
    (glPushMatrix)
    (glDisable GL_TEXTURE_2D)
    (define (yloop y)
      (define (xloop x)
        (when (< x 4)
          (glLoadIdentity)
          (gluLookAt (get-field x player) (+ (get-field y player) 20) 10 0 0 0 0 0 1)
          (glTranslatef (- (* 6 x) 12) (- (* 6 y) 12) -5)
          (case (vector-ref (vector-ref grid y) x)
            ((1) (glColor3f 1 0 0))
            ((0) (glColor3f 1 1 1)))
          (glBegin GL_TRIANGLE_STRIP)
          (glVertex3f 0 0 0)
          (glVertex3f 6 0 0)
          (glVertex3f 0 6 0)
          (glVertex3f 6 6 0)
          (glEnd)
          (xloop (+ x 1))))
      (when (< y 4)
        (xloop 0)
        (yloop (+ y 1))))
    (yloop 0)
    
    (glEnable GL_TEXTURE_2D)
    
    (glTranslatef 0 0 5)
    (glRotatef 90 1 0 0)
    (glColor3f 1 1 1)
    (glBindTexture GL_TEXTURE_2D (gl-vector-ref texlist 0))
    (for-each (lambda (the-tri)
                (glBegin GL_TRIANGLES)
                (glTexCoord2f (tvertex-u (tri-vt1 the-tri)) (tvertex-v (tri-vt1 the-tri)))
                (glVertex3f (vertex-x (tri-v1 the-tri)) (vertex-y (tri-v1 the-tri)) (vertex-z (tri-v1 the-tri)))
                
                (glTexCoord2f (tvertex-u (tri-vt2 the-tri)) (tvertex-v (tri-vt2 the-tri)))
                (glVertex3f (vertex-x (tri-v2 the-tri)) (vertex-y (tri-v2 the-tri)) (vertex-z (tri-v2 the-tri)))
                
                (glTexCoord2f (tvertex-u (tri-vt3 the-tri)) (tvertex-v (tri-vt3 the-tri)))
                (glVertex3f (vertex-x (tri-v3 the-tri)) (vertex-y (tri-v3 the-tri)) (vertex-z (tri-v3 the-tri)))
                (glEnd))
              (vector->list (get-field tris prof)))
    (glPopMatrix)))

(define tick
  (lambda ()
    (send glcanvas refresh)))

(define frame (new frame%  (width 800) (height 600) (label "3d Test for Project Terralay")))
(define prof (load-model "models/prof2.obj"))
(define glcanvas (new glcanvas% (parent frame)))

(send frame show #t)