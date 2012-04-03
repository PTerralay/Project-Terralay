
(set! texture-list (glGenTextures 11))
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
  