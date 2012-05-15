
(define alphabetbitmap (make-object bitmap% "images/alphabet.png" 'png/alpha #f))
(define (texloop filename i number-of-textures texwidth texheight)
  (if (eq? i number-of-textures)
      '()
      (cons (bitmaparea->gl-vector (make-object bitmap% filename 'png/alpha #f) (* i texwidth) 0 texwidth texheight) (texloop filename (+ i 1) number-of-textures texwidth texheight))))
(define tile-texs (list
                   (list 
                    (image->gl-vector "images/floortile.png"))
                   (texloop "images/wall1.png" 0 16 32 32)
                   (list 
                    (image->gl-vector "images/door1.png"))
                   (list 
                    (image->gl-vector "images/floor2.png"))))

(define char-texs (list
                   (texloop "images/prof.png" 0 20 32 64)
                   (list (image->gl-vector "images/monster.png"))
                   (texloop "images/EireSmile.png" 0 20 32 64)
                   (texloop "images/Curious.png" 0 20 32 64)))

(define thing-texs (list
                    (list (image->gl-vector "images/Generator1.png"))
                    (list (image->gl-vector "images/Generator2.png"))
                    (list (image->gl-vector "images/Screwdriver.png"))
                    (list (image->gl-vector "images/SlidedoorLclosed.png"))
                    ))

(set! tile-texture-list (glGenTextures (* (length tile-texs) 16)))

(set! char-texture-list (glGenTextures (* (length char-texs) 20)))
(set! text-texture-list (glGenTextures 70))
(set! thing-texture-list (glGenTextures 10))
(set! texture-list (glGenTextures 4))
(set! char-animations (list 
                       (list (glGenTextures 4) (glGenTextures 4) (glGenTextures 4) (glGenTextures 4)) 
                       (list (glGenTextures 4) (glGenTextures 4) (glGenTextures 4) (glGenTextures 4))))


(glEnable GL_TEXTURE_2D)


(define mask (image->gl-vector "images/mask.png"))


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
(let ((i 0)
      (j 0))
  
  (for-each (lambda (char-list)
              (if (eq? (length char-list) 20)
                  (for-each (lambda (tex)
                              (display i) (display " ") (display j) (newline)
                              (glBindTexture GL_TEXTURE_2D (gl-vector-ref char-texture-list (+ (* i 20) j)))
                              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                              (glTexImage2D GL_TEXTURE_2D 
                                            0 
                                            4 
                                            (list-ref (list-ref (list-ref char-texs i) j) 0) 
                                            (list-ref (list-ref (list-ref char-texs i) j) 1) 
                                            0 
                                            GL_RGBA 
                                            GL_UNSIGNED_BYTE 
                                            (list-ref (list-ref (list-ref char-texs i) j) 2))
                              (set! j (+ j 1)))
                            char-list)
                  (begin
              (glBindTexture GL_TEXTURE_2D (gl-vector-ref char-texture-list (* i 20)))
              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
              (glTexImage2D GL_TEXTURE_2D 
                            0 
                            4 
                            (list-ref (list-ref (list-ref char-texs i) 0) 0) 
                            (list-ref (list-ref (list-ref char-texs i) 0) 1) 
                            0 
                            GL_RGBA 
                            GL_UNSIGNED_BYTE 
                            (list-ref (list-ref (list-ref char-texs i) 0) 2))))
              (set! j 0)
              (set! i (+ i 1)))
            char-texs))

(let ((i 0))
  (for-each (lambda (thing-list)
              (glBindTexture GL_TEXTURE_2D (gl-vector-ref thing-texture-list i))
              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
              (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
              (glTexImage2D GL_TEXTURE_2D 
                            0 
                            4 
                            (list-ref (list-ref (list-ref thing-texs i) 0) 0) 
                            (list-ref (list-ref (list-ref thing-texs i) 0) 1) 
                            0 
                            GL_RGBA 
                            GL_UNSIGNED_BYTE 
                            (list-ref (list-ref (list-ref thing-texs i) 0) 2))
              (set! i (+ i 1)))
            thing-texs))

(glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list 1))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 4 (list-ref mask 0) (list-ref mask 1) 0 GL_RGBA GL_UNSIGNED_BYTE (list-ref mask 2))

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
