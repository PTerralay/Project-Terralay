
(define alphabetbitmap (make-object bitmap% "images/alphabet.png" 'png/alpha #f))

(define tile-texs (list
                   (list 
                    (image->gl-vector "images/floortile.png"))
                   (list 
                    (image->gl-vector "images/wall1.png")
                    (image->gl-vector "images/wall1u.png")
                    (image->gl-vector "images/wall1r.png")
                    (image->gl-vector "images/wall1d.png")
                    (image->gl-vector "images/wall1l.png")
                    (image->gl-vector "images/wall1ur.png")
                    (image->gl-vector "images/wall1rd.png")
                    (image->gl-vector "images/wall1dl.png")
                    (image->gl-vector "images/wall1lu.png")
                    (image->gl-vector "images/wall1ud.png")
                    (image->gl-vector "images/wall1rl.png")
                    (image->gl-vector "images/wall1urd.png")
                    (image->gl-vector "images/wall1rdl.png")
                    (image->gl-vector "images/wall1dlu.png")
                    (image->gl-vector "images/wall1lur.png")
                    (image->gl-vector "images/wall1urdl.png"))
                   (list 
                    (image->gl-vector "images/door1.png"))))
(set! tile-texture-list (glGenTextures (* (length tile-texs) 16)))
(set! text-texture-list (glGenTextures 70))
(set! thing-texture-list (glGenTextures 10))
(set! texture-list (glGenTextures 4))
(set! char-animations (list 
                       (list (glGenTextures 4) (glGenTextures 4) (glGenTextures 4) (glGenTextures 4)) 
                       (list (glGenTextures 4) (glGenTextures 4) (glGenTextures 4) (glGenTextures 4))))


(glEnable GL_TEXTURE_2D)

(define playertex (image->gl-vector "images/player.png"))
(define mask (image->gl-vector "images/mask.png"))
(define tetsytex (image->gl-vector "images/monster.png"))
(define Eiresmiletex (image->gl-vector "images/Eiresmile.png"))


(let ((i 0)
      (j 0))
  (for-each (lambda (tile-list)
              (if (eq? (length tile-list) 16)
                  (for-each (lambda (tex)
                              (glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list (+ (* i 16) j)))
                              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                              (glTexImage2D GL_TEXTURE_2D 
                                            0 
                                            4 
                                            (list-ref (list-ref (list-ref tile-texs i) j) 0) 
                                            (list-ref (list-ref (list-ref tile-texs i) j) 1) 
                                            0 
                                            GL_RGBA 
                                            GL_UNSIGNED_BYTE 
                                            (list-ref (list-ref (list-ref tile-texs i) j) 2))
                              (set! j (+ j 1)))
                            tile-list)
                  (begin
                    (glBindTexture GL_TEXTURE_2D (gl-vector-ref tile-texture-list (* i 16)))
                    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                    (glTexImage2D GL_TEXTURE_2D 
                                  0 
                                  4 
                                  (list-ref (list-ref (list-ref tile-texs i) 0) 0) 
                                  (list-ref (list-ref (list-ref tile-texs i) 0) 1) 
                                  0 
                                  GL_RGBA 
                                  GL_UNSIGNED_BYTE 
                                  (list-ref (list-ref (list-ref tile-texs i) 0) 2))))
              (set! j 0)
              (set! i (+ i 1)))
            tile-texs))

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

(glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 3))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref Eiresmiletex 0) (list-ref Eiresmiletex 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref Eiresmiletex 2))

(define letters (letrec ((loop (λ (i)
                                 (cond ((> i 69) '())
                                       ((> i 34) (cons (bitmaparea->gl-vector alphabetbitmap (+ (* (- i 35) 18) 2) 33 18 31) (loop (+ i 1))))
                                       (else (cons (bitmaparea->gl-vector alphabetbitmap (+ (* i 18) 2) 0 18 31) (loop (+ i 1))))))))
                  
                  (loop 0)))

(letrec ((loop (λ (i)
                 (when (< i 70)
                   (glBindTexture GL_TEXTURE_2D (gl-vector-ref text-texture-list i))
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                   (glTexImage2D GL_TEXTURE_2D 0 4 (list-ref (list-ref letters i) 0) (list-ref (list-ref letters i) 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref (list-ref letters i) 2))
                   (loop (+ i 1))))))
  (loop 0))
