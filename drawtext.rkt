#lang racket/gui
(require sgl/gl sgl/gl-vectors)
(provide draw-text)
;-------------------------------------------------------------------------------
;draw-text:
;  A method that simply prints out text on the screen in the current color.
;params:
; x - the x coord
; y - the y coord 
; scale-factor - a scaling factor where 1 gives text 25 units tall
; a-string - the string to be printed
; texture-list - the global list of textures passed to draw-text.
;-------------------------------------------------------------------------------
(define (draw-text x y scale-factor a-string texture-list)
  (glEnable GL_TEXTURE_2D)
  (glPushMatrix)
  (glTranslatef x y 0)
  (define colnum 0)
  (for-each (lambda (character)
              (if (eq? character #\newline)
                  (begin
                  (glTranslatef (- (* scale-factor 13 (+ colnum 1)))
                                (* 25 scale-factor) 0)
                  
                    (set! colnum 0))
                  (begin
              (let ((char-int (char->integer character)))
                (cond ((and (> char-int 64) (< char-int 91)) 
                       (glBindTexture
                        GL_TEXTURE_2D
                        (gl-vector-ref texture-list (+ (- char-int 65)))))
                      ((and (> char-int 96) (< char-int 123)) 
                       (glBindTexture
                        GL_TEXTURE_2D
                        (gl-vector-ref texture-list (+ (- char-int 71)))))
                      ((and (> char-int 47) (< char-int 58))
                       (glBindTexture
                        GL_TEXTURE_2D
                        (gl-vector-ref texture-list (+ (+ (- char-int 48) 52)))))
                      (else (case character
                              ((#\-)
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 62)))
                              ((#\.)
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 63)))
                              ((#\,)
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 64)))
                              ((#\")
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 65)))
                              ((#\')
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 66)))
                              ((#\!)
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 67)))
                              ((#\?)
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 68)))
                              ((#\space)
                               (glBindTexture
                                GL_TEXTURE_2D
                                (gl-vector-ref texture-list 69)))))))
              (glBegin GL_TRIANGLE_STRIP)
              (glTexCoord2i 0 0)
              (glVertex2f 0 0)
              (glTexCoord2i 1 0)
              (glVertex2f (* scale-factor 13) 0)
              (glTexCoord2i 0 1)
              (glVertex2f 0 (* scale-factor 25))
              (glTexCoord2i 1 1)
              (glVertex2f (* scale-factor 13) (* scale-factor 25))
              (glEnd)
              (set! colnum (+ colnum 1))
              (glTranslatef (* scale-factor 13) 0 0))))
                  
            (string->list a-string))
  (glPopMatrix)
  (glDisable GL_TEXTURE_2D))


