#lang racket/gui
(require sgl/gl)

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
              ((left) (set-field! x player (- (get-field x player) 10)))
              ((right) (set-field! x player (+ (get-field x player) 10)))))))))

(define (gl-init)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  (glClearColor 0 0 0 1)
  (glViewport 0 0 (send glcanvas get-width) (send glcanvas get-height))
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 45 (/ (send glcanvas get-width) (send glcanvas get-height))  1 1000)
  
  (glMatrixMode GL_MODELVIEW)
  (new timer% (interval 20) (notify-callback tick)))

(define gl-draw
  
  (lambda ()
    (glClear GL_COLOR_BUFFER_BIT)
    (glClear GL_DEPTH_BUFFER_BIT)
    
    (define (yloop y)
      (define (xloop x)
        (when (< x 4)
          (glLoadIdentity)
          (gluLookAt (get-field x player) (+ (get-field y player) 20) 40 0 0 0 0 0 1)
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
    (yloop 0)))


(define tick
  (lambda ()
    (send glcanvas refresh)))

(define frame (new frame%  (width 800) (height 600) (label "3d Test for Project Terralay")))

(define glcanvas (new glcanvas% (parent frame)))

(send frame show #t)