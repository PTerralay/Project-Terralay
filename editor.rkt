#lang racket/gui
(require sgl/gl
         sgl/gl-vectors
         "drawtext.rkt" "graphics-utils.rkt")

(define tile-texture-list #f)
(define text-texture-list #f)
(define thing-texture-list #f)
(define texture-list #f)
(define char-animations #f)

(define tile-vectors (make-vector 1 (make-vector 1 99)))

(define map-width 1)
(define map-height 1)
(define editing? #f)

(define current-tile-type 0)


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
         (let ((cursor-grid-x (+ (/ topleftx tile-width) (quotient (send me get-x) tile-width)))
               (cursor-grid-y (+ (/ toplefty tile-width) (quotient (send me get-y) tile-width))))
           (when (and (< cursor-grid-x map-width) 
                      (< cursor-grid-y map-height)
                      (> cursor-grid-x -1)
                      (> cursor-grid-y -1))
             (set-tile! cursor-grid-x cursor-grid-y current-tile-type)
             (set-tile! cursor-grid-x (- cursor-grid-y 1) 'same)
             (set-tile! (+ cursor-grid-x 1) cursor-grid-y 'same)
             (set-tile! cursor-grid-x (+ cursor-grid-y 1) 'same)
             (set-tile! (- cursor-grid-x 1) cursor-grid-y 'same))))
        
        ((right-down)
         (let ((cursor-grid-x (+ (/ topleftx tile-width) (quotient (send me get-x) tile-width)))
               (cursor-grid-y (+ (/ toplefty tile-width) (quotient (send me get-y) tile-width))))
           (when (and (< cursor-grid-x map-width) 
                      (< cursor-grid-y map-height)
                      (> cursor-grid-x -1)
                      (> cursor-grid-y -1))
             (let ((the-row (vector-ref tile-vectors cursor-grid-y)))
               (vector-set! the-row cursor-grid-x 99)
               (vector-set! tile-vectors cursor-grid-y the-row)))))))
    
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
         (bottomrightx (+ topleftx (send glcanvas get-width)))
         (bottomrighty (+ toplefty (send glcanvas get-height))))
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
                  (glColor4f 1 1 1 1)
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
                  (glEnable GL_TEXTURE_2D)
                  (glMatrixMode GL_PROJECTION)
                  (glPushMatrix)
                  
                  (glColor4f 1 1 1 1)
                  (if (eq? (vector-ref (vector-ref tile-vectors y) x) 99)
                      (glColor4f 0 0 0 1)
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
                  
                  (glPopMatrix)
                  (set! y (+ 1 y))
                  (yloop)))
              (yloop)
              (set! x (+ 1 x))
              (xloop))))
        (xloop)))))

(define (checkneighbours x y up right down left type)
  (and (if up
           (and (not (eq? y 0)) 
                (> (vector-ref (vector-ref tile-vectors (- y 1)) x) (* (- type 1) 16))
                (< (vector-ref (vector-ref tile-vectors (- y 1)) x) (+ (* type 16) 1)))
           #t)
       (if right
           (and (not (eq? x (- map-width 1))) 
                (> (vector-ref (vector-ref tile-vectors y) (+ x 1)) (* (- type 1) 16))
                (< (vector-ref (vector-ref tile-vectors y) (+ x 1)) (+ (* type 16) 1)))
           #t)
       (if down
           (and (not (eq? y (- map-height 1))) 
                (> (vector-ref (vector-ref tile-vectors (+ y 1)) x) (* (- type 1) 16))
                (< (vector-ref (vector-ref tile-vectors (+ y 1)) x) (+ (* type 16) 1)))
           #t)
       (if left
           (and (not (eq? x 0)) 
                (> (vector-ref (vector-ref tile-vectors y) (- x 1)) (* (- type 1) 16))
                (< (vector-ref (vector-ref tile-vectors y) (- x 1)) (+ (* type 16) 1)))
           #t)))


(define (set-tile! x y type)
  (unless (or (< x 0) (< y 0) (eq? x map-width) (eq? y map-height))
    (when (eq? type 'same)
      (set! type (vector-ref (vector-ref tile-vectors y) x)))
    (case type
      ((0) (let ((the-row (vector-ref tile-vectors y)))
             (vector-set! the-row x 0)
             (vector-set! tile-vectors y the-row)))
      ((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) (let ((the-row (vector-ref tile-vectors y)))
             (cond
               ;all four
               ((checkneighbours x y #t #t #t #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 15 1))
                (vector-set! tile-vectors y the-row))
               ;not left
               ((checkneighbours x y #t #t #t #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 11 1))
                (vector-set! tile-vectors y the-row))
               ;not up
               ((checkneighbours x y #f #t #t #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 12 1))
                (vector-set! tile-vectors y the-row))
               ;not right
               ((checkneighbours x y #t #f #t #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 13 1))
                (vector-set! tile-vectors y the-row))
               ;not down
               ((checkneighbours x y #t #t #f #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 14 1))
                (vector-set! tile-vectors y the-row))
               
               ;not down and left
               ((checkneighbours x y #t #t #f #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 5 1))
                (vector-set! tile-vectors y the-row))
               ;not left and up
               ((checkneighbours x y #f #t #t #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 6 1))
                (vector-set! tile-vectors y the-row))
               ;not up and right
               ((checkneighbours x y #f #f #t #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 7 1))
                (vector-set! tile-vectors y the-row))
               ;not right and down
               ((checkneighbours x y #t #f #f #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 8 1))
                (vector-set! tile-vectors y the-row))
               ;not left and right
               ((checkneighbours x y #t #f #t #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 9 1))
                (vector-set! tile-vectors y the-row))
               ;not up and down
               ((checkneighbours x y #f #t #f #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 10 1))
                (vector-set! tile-vectors y the-row))
               ;only up
               ((checkneighbours x y #t #f #f #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 1 1))
                (vector-set! tile-vectors y the-row))
               ;only right
               ((checkneighbours x y #f #t #f #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 2 1))
                (vector-set! tile-vectors y the-row))
               ;only down
               ((checkneighbours x y #f #f #t #f type)
                (vector-set! the-row x (+ (* (- type 1) 16) 3 1))
                (vector-set! tile-vectors y the-row))
               ;only left
               ((checkneighbours x y #f #f #f #t type)
                (vector-set! the-row x (+ (* (- type 1) 16) 4 1))
                (vector-set! tile-vectors y the-row))
               (else
                (vector-set! the-row x (+ (* (- type 1) 16) 1))
                (vector-set! tile-vectors y the-row)))))
      
      ((17) (let ((the-row (vector-ref tile-vectors y)))
             (vector-set! the-row x 17)
             (vector-set! tile-vectors y the-row))))))







(define (save output)
  (display "Saving")
  (let ((i 0)
        (ylist (vector->list tile-vectors)))
    (for-each (lambda (row-vector)
                (let ((xlist (vector->list row-vector)))
                  (for-each (lambda (type)
                              ;                              (when (eq? type #f)
                              ;                                (set! type 99))
                              (let ((10digit (quotient type 10))
                                    (1digit (remainder type 10)))
                                (write 10digit output )
                                (write 1digit  output )))
                            xlist)
                  (when (< i (- (length ylist) 1))
                    
                    (newline output))
                  (set! i (+ i 1))))
              ylist))
  (close-output-port output))


(define (load-map data-file)
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
                  ;                  (when (eq? tile-candidate 99)
                  ;                    (set! tile-candidate #f))
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

(define (resize-map)
  
  (define new-grid (make-vector map-height #f))
  (define (gridloop rownum)
    (when (< rownum map-height)
      (vector-set! new-grid rownum (make-vector map-width 99))
      (gridloop (+ rownum 1))))
  (gridloop 0)
  (display tile-vectors)
  (newline)
  (display new-grid)
  (newline)
  (define (yloop rowindex)
    (when (and (< rowindex (vector-length tile-vectors))
               (< rowindex map-height))
      (let ((row-candidate (make-vector map-width 99)))
        (define (xloop colindex)
          (when (and (< colindex (vector-length (vector-ref tile-vectors 0)))
                     (< colindex map-width))
            (vector-set! row-candidate colindex (vector-ref (vector-ref tile-vectors rowindex) colindex))
            (xloop (+ colindex 1))))
        (xloop 0)
        (vector-set! new-grid rowindex row-candidate))
      (yloop (+ rowindex 1))))
  (yloop 0)
  (set! tile-vectors new-grid)
  (display tile-vectors))









(define frame (new frame% 
                   (width 800) 
                   (height 600)
                   (label "Project Terralay map editor")))

(define panel (new horizontal-panel%
                   (parent frame)
                   (alignment '(left center))
                   (stretchable-height #f)))

(define panel2 (new horizontal-panel%
                    (parent frame)
                    (alignment '(left center))
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
                   (set! tile-vectors (load-map input))
                   (set! map-height (vector-length tile-vectors))
                   (set! map-width (vector-length (vector-ref tile-vectors 0)))
                   
                   (display tile-vectors)))))

(define coords (new message%
                    (parent panel)
                    (label "No map selected")
                    (min-width 130)))



(define tile-type-chooser (new choice%
                               (parent panel)
                               (label "Tile Type")
                               (stretchable-width #f)
                               (choices (dynamic-require "tile-types.rkt" 'tile-types))
                               (callback (lambda (c e)
                                           (set! current-tile-type (send c get-selection))))))

(define fill-type-chooser 
  (new choice%
       (parent panel)
       (label "Fill map with tile")
       (stretchable-width #f)
       (choices (dynamic-require "tile-types.rkt" 'tile-types))
       (callback (lambda (c e)
                   (when (eq? (message-box/custom "Confirm filling the map"
                                                  (string-append "Do you want to erase the map and fill it with '" (send c get-string-selection) "'?")
                                                  "Fill"
                                                  "Cancel"
                                                  #f
                                                  frame
                                                  '(disallow-close caution default=2))
                              1) 
                     (define (gridloop rownum)
                       (when (< rownum map-height)
                         (vector-set! tile-vectors rownum (make-vector map-width (send c get-selection)))
                         (gridloop (+ rownum 1))))
                     (gridloop 0))))))


(new button%
     (parent panel)
     (label "Erase map")
     (callback (lambda (b e)
                 (when (eq? (message-box/custom "Confirm erasing the map"
                                                "Do you want to erase the map and fill it with holes?"
                                                "Erase"
                                                "Cancel"
                                                #f
                                                frame
                                                '(disallow-close caution default=2))
                            1)
                   (define (gridloop rownum)
                     (when (< rownum map-height)
                       (vector-set! tile-vectors rownum (make-vector map-width 99))
                       (gridloop (+ rownum 1))))
                   (gridloop 0)))))

(new button%
     (parent panel2)
     (label "+Width")
     (callback (lambda (b e)
                 (set! map-width (+ map-width 1))
                 (resize-map))))
(new button%
     (parent panel2)
     (label "-Width")
     (callback (lambda (b e)
                 (when (> map-width 1)
                   (set! map-width (- map-width 1))
                   (resize-map)))))
(new button%
     (parent panel2)
     (label "+Height")
     (callback (lambda (b e)
                 (set! map-height (+ map-height 1))
                 (resize-map))))
(new button%
     (parent panel2)
     (label "-Height")
     (callback (lambda (b e)
                 (when (> map-height 1)
                   (set! map-height (- map-height 1))
                   (resize-map)))))

(send frame show #t)