#lang racket/gui

(require sgl/gl sgl/gl-vectors)
(provide draw-text)

(define (draw-text a-string texture-list)
  (glColor4f 1 1 1 1)
  (glBegin GL_TRIANGLES)
  (glVertex2f 0 0 )
  (glVertex2f 100 100)
  (glVertex2f 0 100)
  (glEnd)
  (glEnable GL_TEXTURE_2D)
  (for-each (lambda (character)
              (let ((char-int (char->integer character)))
                (if (or (and (> char-int 64) (< char-int 91))
                        (and (> char-int 96) (< char-int 123))) 
                    (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ (- char-int 65) 12)))
                    (case character
                      ((#\space) (glBindTexture GL_TEXTURE_2D (gl-vector-ref texture-list (+ 26 12)))))))
              (glBegin GL_TRIANGLE_STRIP)
              (glTexCoord2i 0 0)
              (glVertex2i 0 0)
              (glTexCoord2i 1 0)
              (glVertex2i 180 0)
              (glTexCoord2i 0 1)
              (glVertex2i 0 310)
              (glTexCoord2i 1 1)
              (glVertex2i 180 310)
              (glEnd)
              (glTranslatef 180 0 0))
            
            (string->list a-string))
  (glDisable GL_TEXTURE_2D))


