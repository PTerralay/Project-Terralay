
;__________________________________________________________________________________
;              Image loading function, based on example code bundled with drracket
;----------------------------------------------------------------------------------
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

(define (image->gl-vector file) 
  (bitmap->gl-vector (make-object bitmap% file 'png/alpha #f)))

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
    (list (send bmp get-width) (send bmp get-height) vec)))


(define (imagearea->gl-vector the-bitmap x y width height) 
  (bitmaparea->gl-vector the-bitmap x y width height))

(define alphabetbitmap (make-object bitmap% "images/testbet.png" 'png/alpha #f))

(set! texture-list (glGenTextures (+ 12 58)))
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
(define tetsytex (image->gl-vector "images/monster.png"))


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

(glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 11))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref tetsytex 0) (list-ref tetsytex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref tetsytex 2))

(define letters (letrec ((loop (λ (i)
                                 (if (> i 57)
                                     '()
                                     (if (> i 28)
                                         (cons (imagearea->gl-vector alphabetbitmap (+ (* (- i 29) 18) 2) 32 18 31) (loop (+ i 1)))
                                         (cons (imagearea->gl-vector alphabetbitmap (+ (* i 19) 2) 0 18 31) (loop (+ i 1))))))))
                  (loop 0)))

(letrec ((loop (λ (i)
                 (when (< i 58)
                   (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ i 12)))
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                   (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref (list-ref letters i) 0) (list-ref (list-ref letters i) 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref (list-ref letters i) 2))
                   (loop (+ i 1))))))
  (loop 0))
