#lang racket/gui
(require sgl/gl
         sgl/gl-vectors
         "drawtext.rkt" "graphics-utils.rkt")

(define tile-texture-list #f)
(define text-texture-list #f)
(define thing-texture-list #f)
(define texture-list #f)
(define char-animations #f)

(struct tile ((family #:mutable) (type #:mutable #:auto))
  #:auto-value 0)

(define tile-vectors (make-vector 1 (make-vector 1 (tile -1))))

(define map-width 1)
(define map-height 1)
(define editing? #f)

(define current-tile-family 0)



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
    (define/private (react-to-mouse btn me)
      (if (eq? btn 'left)
          (let ((cursor-grid-x (+ (/ topleftx tile-width) (quotient (send me get-x) tile-width)))
                (cursor-grid-y (+ (/ toplefty tile-width) (quotient (send me get-y) tile-width))))
            (when (and (< cursor-grid-x (- map-width 1)) 
                       (< cursor-grid-y (- map-height 1))
                       (> cursor-grid-x 0)
                       (> cursor-grid-y 0))
              (set&update-tile! cursor-grid-x cursor-grid-y current-tile-family)
              (set&update-tile! cursor-grid-x (- cursor-grid-y 1) 'same)
              (set&update-tile! (+ cursor-grid-x 1) cursor-grid-y 'same)
              (set&update-tile! cursor-grid-x (+ cursor-grid-y 1) 'same)
              (set&update-tile! (- cursor-grid-x 1) cursor-grid-y 'same)))
          (let ((cursor-grid-x (+ (/ topleftx tile-width) (quotient (send me get-x) tile-width)))
                (cursor-grid-y (+ (/ toplefty tile-width) (quotient (send me get-y) tile-width))))
            (when (and (< cursor-grid-x (- map-width 1)) 
                       (< cursor-grid-y (- map-height 1))
                       (> cursor-grid-x 0)
                       (> cursor-grid-y 0))
              (set&update-tile! cursor-grid-x cursor-grid-y -1)
              (set&update-tile! cursor-grid-x (- cursor-grid-y 1) 'same)
              (set&update-tile! (+ cursor-grid-x 1) cursor-grid-y 'same)
              (set&update-tile! cursor-grid-x (+ cursor-grid-y 1) 'same)
              (set&update-tile! (- cursor-grid-x 1) cursor-grid-y 'same)))))
    (define/override (on-event me)
      (case (send me get-event-type)
        ((motion)
         (cond ((send me get-left-down)
                (react-to-mouse 'left me))
               ((send me get-right-down)
                (react-to-mouse 'right me))))
        ((left-down)
         (react-to-mouse 'left me))
        
        ((right-down)
         (react-to-mouse 'right me))))
    
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
  (send coords set-label (string-append "Map size: " (number->string map-width) "x" (number->string map-height)))
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
                  (if (eq? (tile-family (vector-ref (vector-ref tile-vectors y) x)) -1)
                      (glColor4f 0 0 0 1)
                      (glBindTexture 
                       GL_TEXTURE_2D 
                       (gl-vector-ref tile-texture-list 
                                      (+ (* (tile-family (vector-ref (vector-ref tile-vectors y) x)) 16) 
                                         (tile-type (vector-ref (vector-ref tile-vectors y) x))))))
                  
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

(define (checkneighbours x y up right down left family)
  (and (if up
           (and (not (eq? y 0)) 
                (eq? (tile-family (vector-ref (vector-ref tile-vectors (- y 1)) x)) family))
           
           #t)
       (if right
           (and (not (eq? x (- map-width 1))) 
                (eq? (tile-family (vector-ref (vector-ref tile-vectors y) (+ x 1))) family))
           #t)
       (if down
           (and (not (eq? y (- map-height 1))) 
                (eq? (tile-family (vector-ref (vector-ref tile-vectors (+ y 1)) x)) family))
           #t)
       (if left
           (and (not (eq? x 0)) 
                (eq? (tile-family (vector-ref (vector-ref tile-vectors y) (- x 1))) family))
           #t)))


(define (set&update-tile! x y family)
  (unless (or (< x 0) (< y 0) (eq? x map-width) (eq? y map-height))
    (let ((thetile (vector-ref (vector-ref tile-vectors y) x)))
      (when (eq? family 'same)
        (set&update-tile! x y (tile-family thetile)))
      (case family
        ((-1) (set-tile-family! thetile -1))
        ((0) (set-tile-family! thetile 0)
             (set-tile-type! thetile 0))
        ((3) (set-tile-family! thetile 3)
             (set-tile-type! thetile 0))
        ((1) (set-tile-family! thetile 1) 
             (cond
               ;all four
               ((checkneighbours x y #t #t #t #t family)
                (set-tile-type! thetile 15))
               ;not left
               ((checkneighbours x y #t #t #t #f family)
                (set-tile-type! thetile 11))
               ;not up
               ((checkneighbours x y #f #t #t #t family)
                (set-tile-type! thetile 12))
               ;not right
               ((checkneighbours x y #t #f #t #t family)
                (set-tile-type! thetile 13))
               ;not down
               ((checkneighbours x y #t #t #f #t family)
                (set-tile-type! thetile 14))
               
               ;not down and left
               ((checkneighbours x y #t #t #f #f family)
                (set-tile-type! thetile 5))
               ;not left and up
               ((checkneighbours x y #f #t #t #f family)
                (set-tile-type! thetile 6))
               ;not up and right
               ((checkneighbours x y #f #f #t #t family)
                (set-tile-type! thetile 7))
               ;not right and down
               ((checkneighbours x y #t #f #f #t family)
                (set-tile-type! thetile 8))
               ;not left and right
               ((checkneighbours x y #t #f #t #f family)
                (set-tile-type! thetile 9))
               ;not up and down
               ((checkneighbours x y #f #t #f #t family)
                (set-tile-type! thetile 10))
               ;only up
               ((checkneighbours x y #t #f #f #f family)
                (set-tile-type! thetile 1))
               ;only right
               ((checkneighbours x y #f #t #f #f family)
                (set-tile-type! thetile 2))
               ;only down
               ((checkneighbours x y #f #f #t #f family)
                (set-tile-type! thetile 3))
               ;only left
               ((checkneighbours x y #f #f #f #t family)
                (set-tile-type! thetile 4))
               (else
                (set-tile-type! thetile 0))))
        
        ((2) (set-tile-family! thetile 2)
             (set-tile-type! thetile 0))))))







(define (save output)
  (display "Saving")
  (let ((i 0)
        (ylist (vector->list tile-vectors)))
    (for-each (lambda (row-vector)
                (let ((xlist (vector->list row-vector)))
                  (for-each (lambda (thetile)
                              (let ((type-number (+ (* (tile-family thetile) 16) 
                                                    (tile-type thetile))))
                                (when (eq? (tile-family thetile) -1)
                                  (set! type-number 999))
                                (let ((100digit (quotient type-number 100))
                                      (10digit (quotient type-number 10))
                                      (1digit (remainder type-number 10)))
                                  (when (> 10digit 10)
                                    (set! 10digit (- 10digit (* 100digit 10))))
                                  (write 100digit output )
                                  (write 10digit output )
                                  (write 1digit  output ))))
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
                       (data3 (read-char data-file))
                       (type-number (+ (* (string->number (string data1)) 100)
                                       (* (string->number (string data2)) 10)
                                       (string->number (string data3))))
                       (tile-candidate (tile (quotient type-number 16))))
                  (set-tile-type! tile-candidate (remainder type-number 16))
                  (when (eq? type-number 999)
                    (set-tile-family! tile-candidate -1)
                    (set-tile-type! tile-candidate 0))
                  
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
      (define new-row (make-vector map-width #f))
      (define (rowloop colnum)
        (when (< colnum map-width)
          (vector-set! new-row colnum (tile -1))
          (rowloop (+ colnum 1))))
      (rowloop 0)
      (vector-set! new-grid rownum new-row)
      (gridloop (+ rownum 1))))
  (gridloop 0)
  (define (yloop rowindex)
    (when (and (< rowindex (vector-length tile-vectors))
               (< rowindex map-height))
      (let ((row-candidate (make-vector map-width (tile -1))))
        (define (xloop colindex)
          (when (and (< colindex (vector-length (vector-ref tile-vectors 0)))
                     (< colindex map-width))
            (vector-set! row-candidate colindex (vector-ref (vector-ref tile-vectors rowindex) colindex))
            (xloop (+ colindex 1))))
        (xloop 0)
        (vector-set! new-grid rowindex row-candidate))
      (yloop (+ rowindex 1))))
  (yloop 0)
  (set! tile-vectors new-grid))









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
                                           (set! current-tile-family (send c get-selection))))))




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
                       (define new-row (make-vector map-width #f))
                       (define (rowloop colnum)
                         (when (< colnum map-width)
                           (vector-set! new-row colnum (tile -1))
                           (rowloop (+ colnum 1))))
                       (rowloop 0)
                       (vector-set! tile-vectors rownum new-row)
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
(define fill-type-chooser 
  (new choice%
       (parent panel2)
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
                         (define new-row (make-vector map-width #f))
                         (define (rowloop colnum)
                           (when (< colnum map-width)
                             (if (and (> rownum 0) (> colnum 0) (< rownum (- map-height 1)) (< colnum (- map-width 1)))
                                 (vector-set! new-row colnum (tile (send c get-selection)))
                                 (vector-set! new-row colnum (tile -1)))
                             (rowloop (+ colnum 1))))
                         (rowloop 0)
                         (vector-set! tile-vectors rownum new-row)
                         (gridloop (+ rownum 1))))
                     
                     (gridloop 0))))))

(send frame show #t)