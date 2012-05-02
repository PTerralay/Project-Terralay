#lang racket/gui
(require sgl/gl sgl/gl-vectors)
(provide (all-defined-out))


;------------------------------------------------------------------------------
;bitmap->gl-vector: Image loading function, based on example code bundled with drracket.
;params: 
; bmp - a texture bitmap
;------------------------------------------------------------------------------
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


;------------------------------------------------------------------------------
;image->gl-vector: Takes a file and returns a OpenGL texture.
;params:
; file - the file containing the image
;------------------------------------------------------------------------------
(define (image->gl-vector file) 
  (bitmap->gl-vector (make-object bitmap% file 'png/alpha #f)))

;------------------------------------------------------------------------------
;bitmaparea->gl-vector: Takes a bitmap and an area and returns a vector with the
; bitmap data for only that area, this way we can have different textures in the same file.
;params: 
; bmp - the entire bitmap from a file
; x - x-coord of the top-left corner
; y - y-coord of the top-left corner
; width - the width of the area
; height - the height of the area.
;------------------------------------------------------------------------------
(define (bitmaparea->gl-vector bmp x y width height)
  (let* ((dc (instantiate bitmap-dc% (bmp)))
         (pixels (* width height))
         (vec (make-gl-ubyte-vector (* pixels 4)))
         (data (make-bytes (* pixels 4)))
         (i 0))
    (send dc get-argb-pixels x y width height data)
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
    (list width height vec)))

