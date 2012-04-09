#lang racket/gui

(require sgl/gl sgl/gl-vectors)
(provide draw-text)

(define (draw-text x y a-string texture-list)
  (glEnable GL_TEXTURE_2D)
  (glPushMatrix)
  (glTranslatef x y 0)
  (for-each (lambda (character)
              (let ((char-int (char->integer character)))
                (cond ((and (> char-int 64) (< char-int 91)) 
                       (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ (- char-int 65) 12))))
                      ((and (> char-int 96) (< char-int 123)) 
                       (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ (- char-int 71) 12)))) 
                      (else (case character
                              ((#\space) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 52 12))))
                              ((#\.) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 53 12))))
                              ((#\,) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 54 12))))
                              ((#\") (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 55 12))))
                              ((#\!) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 56 12))))
                              ((#\?) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 57 12))))))))
              (glBegin GL_TRIANGLE_STRIP)
              (glTexCoord2i 0 0)
              (glVertex2i 0 0)
              (glTexCoord2i 1 0)
              (glVertex2i 13 0)
              (glTexCoord2i 0 1)
              (glVertex2i 0 25)
              (glTexCoord2i 1 1)
              (glVertex2i 13 25)
              (glEnd)
              (glTranslatef 13 0 0))
            
            (string->list a-string))
  (glPopMatrix)
  (glDisable GL_TEXTURE_2D))


