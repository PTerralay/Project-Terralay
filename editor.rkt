#lang racket/gui
(require sgl/gl
         sgl/gl-vectors
         "drawtext.rkt" "graphics-utils.rkt")

(define tile-texture-list #f)
(define text-texture-list #f)
(define thing-texture-list #f)
(define texture-list #f)
(define char-animations #f)

(define tile-vectors #f)

(define map-width 0)
(define map-height 0)
(define editing? #f)

(define (gridloop rownum)
  (when (< rownum map-height)
    (vector-set! tile-vectors rownum (make-vector map-width 0))
    (gridloop (+ rownum 1))))



(define glcanvas%
  (class canvas%
    (inherit with-gl-context refresh swap-gl-buffers)
    (super-new (style '(gl)))
    (define initialized #f)
    (field (topleftx 0)
           (toplefty 0)
           (tile-width 32))
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless initialized
           (gl-init) 
           (set! initialized #t))
         (gl-draw)
         (swap-gl-buffers))))
    
    (define/override (on-event me)
      (case (send me get-event-type) 
        ((left-down)
         (display "CLICK"))
        ((right-down)
         (display "DELETED"))))
    
    (define/override (on-char ke)
      (if (eq? (send ke get-key-code) 'release)
          (void)
          (begin
            (case (send ke get-key-code)
              ((left) (set! topleftx (- topleftx tile-width)))
              ((right) (set! topleftx (+ topleftx tile-width)))
              ((up) (set! toplefty (- toplefty tile-width)))
              ((down) (set! toplefty (+ toplefty tile-width)))
              ((escape) (exit))))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (gl-resize width height))))))

(define (gl-init)
  ;The main game timer.
  (new timer% (interval 100) (notify-callback tick))
  
  (glDisable GL_DEPTH_TEST)
  (include "setuptextures.rkt")
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  
  
  
  (glClearColor 0 0 0 1)
  (glViewport 0 0 (send glcanvas get-width) (send glcanvas get-height))
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity))

(define (gl-resize width height)
  (glViewport 0 0 width height)
  (send glcanvas refresh))

(define (tick)
  (send coords set-label (string-append "from (" 
                                        (number->string (/ (get-field topleftx glcanvas) (get-field tile-width glcanvas))) 
                                        "," 
                                        (number->string (/ (get-field toplefty glcanvas) (get-field tile-width glcanvas)))
                                        ") to ("
                                        (number->string (+ (/ (get-field toplefty glcanvas) (get-field tile-width glcanvas)) 30))
                                        ","
                                        (number->string (+ (/ (get-field topleftx glcanvas) (get-field tile-width glcanvas)) 20))
                                        ")"))
  (send glcanvas refresh))

(define (gl-draw)
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (let* ((tile-width (get-field tile-width glcanvas))
         (topleftx (get-field topleftx glcanvas))
         (toplefty (get-field toplefty glcanvas))
         (bottomrightx (+ topleftx (* 30 tile-width)))
         (bottomrighty (+ toplefty (* 20 tile-width))))
    (glOrtho topleftx bottomrightx bottomrighty toplefty -1 1)
    (glColor4f 1 1 1 1)
    (when tile-vectors
      (let ((tile-width (get-field tile-width glcanvas))
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
                  (if (eq? (vector-ref (vector-ref tile-vectors y) x) #f)
                      (glColor3f 0 0 0)
                      (glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list (vector-ref (vector-ref tile-vectors y) x))))
                  
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
                  (glColor3f 1 1 1)
                  (glBegin GL_LINES)
                  (glVertex2i 0 0)
                  (glVertex2i tile-width 0)
                  (glVertex2i tile-width tile-width)
                  (glVertex2i 0 tile-width)
                  (glEnd)
                  (glPopMatrix)
                  (set! y (+ 1 y))
                  (yloop)))
              (yloop)
              (set! x (+ 1 x))
              (xloop))))
        (xloop)))))


(define (save output)
  (display "Saving")
  (let ((i 0)
        (ylist (vector->list tile-vectors)))
    (for-each (lambda (row-vector)
                (let ((xlist (vector->list row-vector)))
                  (for-each (lambda (type)
                              (let ((10digit (quotient type 10))
                                    (1digit (remainder type 10)))
                                (write 10digit output )
                                (write 1digit  output )))
                            xlist)
                  (when (< i (- (length ylist) 1))
                    
                    (write-char #\return output )
                    (write-char #\newline output ))
                  (set! i (+ i 1))))
              ylist))
  (close-output-port output))


(define (load data-file)
  (let ((iy 0)
        (y-vector '()))    
    (define (y-loop)
      (let ((ix 0)
            (x-vector '()))
        (define (x-loop)
          (let ((data1 (read-char data-file)))
            (when (and (eq? data1 #\return)
                       (eq? (peek-char data-file) #\newline))
              (read-char data-file)); If the sequence \r\n is encountered, the reader is simply incremented
            (if (or (eq? data1 #\return) (eq? data1 #\newline) (eof-object? data1))
                (list->vector (reverse x-vector))
                (let* ((data2 (read-char data-file))
                       (tile-candidate (+ (* (string->number (string data1)) 10)
                                          (string->number (string data2)))))
                  (set! x-vector (cons tile-candidate x-vector))
                  (set! ix (+ ix 1))
                  (x-loop)))))
        (let ((vector-candidate (x-loop)))
          (set! y-vector (cons vector-candidate y-vector))
          (set! iy (+ iy 1))
          (if (eof-object? (peek-char data-file))
              (begin (close-input-port data-file)
                     (list->vector (reverse y-vector)))
              (y-loop)))))
    (y-loop)))

(define frame (new frame% 
                   (width 800) 
                   (height 600)
                   (label "Project Terralay map editor")))

(define panel (new horizontal-panel%
                   (parent frame)
                   (alignment '(left top))
                   (stretchable-height #f)))


(new button% 
     (parent panel) 
     (label "Save map") 
     (callback (lambda (b e)
                 (define file (get-file))
                 (when file
                   (define output (open-output-file file #:mode 'text #:exists 'truncate))
                   (save output)))))

(define glcanvas (new glcanvas%
                      (parent frame)))


(new button%
     (parent panel)
     (label "Load map")
     (callback (lambda (b e)
                 (define file (get-file))
                 (when file
                   (define input (open-input-file file #:mode 'text))
                   (set! tile-vectors (load input))
                   (set! map-height (vector-length tile-vectors))
                   (set! map-width (vector-length (vector-ref tile-vectors 0)))
                   
                   (display tile-vectors)))))

(define coords (new message%
                    (parent panel)
                    (label "No map selected")
                    (min-width 200)))
(define tile-type-chooser (new choice%
     (parent panel)
     (label "Tile Type")
     (choices (dynamic-require "tile-types.rkt" 'tile-types))))



(send frame show #t)