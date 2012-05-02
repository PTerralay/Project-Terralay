
(define alphabetbitmap (make-object bitmap% "images/alphabet.png" 'png/alpha #f))
(set! tile-texture-list (glGenTextures 9))
(set! text-texture-list (glGenTextures 70))
(set! thing-texture-list (glGenTextures 10))
(set! texture-list (glGenTextures 3))
(set! char-animations (list 
                       (list (glGenTextures 4) (glGenTextures 4) (glGenTextures 4) (glGenTextures 4)) 
                       (list (glGenTextures 4) (glGenTextures 4) (glGenTextures 4) (glGenTextures 4))))


(glEnable GL_TEXTURE_2D)
(define tile-texs (list
                   (image->gl-vector "images/floortile.png")
                   (image->gl-vector "images/walltileleft.png")
                   (image->gl-vector "images/walltileright.png")
                   (image->gl-vector "images/walltiletop.png")
                   (image->gl-vector "images/walltilebottom.png")
                   (image->gl-vector "images/specwallleft.png")
                   (image->gl-vector "images/specwallright.png")
                   (image->gl-vector "images/wallcornerbotleft.png")
                   (image->gl-vector "images/wallcornerbotright.png"))
  
(define playertex (image->gl-vector "images/player.png"))
(define mask (image->gl-vector "images/mask.png"))
(define tetsytex (image->gl-vector "images/monster.png"))


(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 0))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref floortex 0) (list-ref floortex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref floortex 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 1))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexleft 0) (list-ref walltexleft 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexleft 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 2))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexright 0) (list-ref walltexright 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexright 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 3))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltextop 0) (list-ref walltextop 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltextop 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 4))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexbot 0) (list-ref walltexbot 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexbot 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 5))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexspecleft 0) (list-ref walltexspecleft 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexspecleft 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 6))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexspecright 0) (list-ref walltexspecright 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexspecright 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 7))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexcornerbotl 0) (list-ref walltexcornerbotl 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexcornerbotl 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list 8))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref walltexcornerbotr 0) (list-ref walltexcornerbotr 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref walltexcornerbotr 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 0))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref playertex 0) (list-ref playertex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref playertex 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 1))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref mask 0) (list-ref mask 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref mask 2))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 2))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref tetsytex 0) (list-ref tetsytex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref tetsytex 2))

(define letters (letrec ((loop (λ (i)
                                 (if (< i 70)
                                     (if (> i 34)
                                         (cons (bitmaparea->gl-vector alphabetbitmap (+ (* (- i 35) 18) 2) 33 18 31) (loop (+ i 1)))
                                         (cons (bitmaparea->gl-vector alphabetbitmap (+ (* i 18) 2) 0 18 31) (loop (+ i 1))))
                                     '()))))
                  (loop 0)))

(letrec ((loop (λ (i)
                 (when (< i 70)
                   (glBindTexture GL_TEXTURE_2D (gl-vector-ref text-texture-list i))
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                   (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref (list-ref letters i) 0) (list-ref (list-ref letters i) 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref (list-ref letters i) 2))
                   (loop (+ i 1))))))
  (loop 0))
