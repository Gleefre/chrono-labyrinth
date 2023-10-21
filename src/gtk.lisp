(in-package #:chrono-labyrinth)

(defun saw (len x)
  (abs
   (- (/ (mod x len) (/ len 2))
      1)))

(defun init-env (env width height y-axis)
  (setf (s::env-programs env) (kit.gl.shader:compile-shader-dictionary 's::sketch-programs)
        (s::env-view-matrix env) (if (eq y-axis :down)
                                     (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                     (kit.glm:ortho-matrix 0 width 0 height -1 1))
        (s::env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
        (s::env-vao env) (make-instance 'kit.gl.vao:vao :type 's::sketch-vao)
        (s::env-white-pixel-texture env) (s::make-white-pixel-texture)
        (s::env-white-color-vector env) #(255 255 255 255)
        (s::env-pen env) (s::make-default-pen)
        (s::env-font env) (s::make-default-font))
  (kit.gl.shader:use-program (s::env-programs env) :fill-shader)
  (kit.gl.shader:uniform-matrix
   (s::env-programs env) :view-m 4 (vector (s::env-view-matrix env))))

(let ((x 0))
  (defun draw (clock)
    (s:background (s:rgb 0 0 1))
    (s:with-pen (s:make-pen :fill (s:rgb (saw 3 (sc:time clock)) 1 0))
      (s:rect 100 100 200 100)
      (s:with-translate (300 (+ 200 (* 50 (saw 7 (sc:time clock)))))
        (s:circle 0 0 30))
      (s:text (format nil "Foo ~A" (incf x)) 50 50))))

(defun example-window-gl-area ()
  (gtk:within-main-loop
    (let ((window (gtk:gtk-window-new :toplevel))
          (box (gtk:gtk-box-new :vertical 10))
          (label (gtk:gtk-label-new "Test"))
          (area (make-instance 'gtk:gtk-gl-area :auto-render t
                                                :width-request 400
                                                :height-request 400))
          (clock (sc:make-clock)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g:g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Bye!~%")
                            (gtk:leave-gtk-main)))
      (gtk:gtk-container-add box label)
      (gobject:g-signal-connect area "render"
                                (let (env)
                                  (lambda (widget context)
                                    (declare (ignore context))
                                    (unless env
                                      (sdl2-ttf:init)
                                      (setf env (s::make-env))
                                      (init-env env 400 400 :up)
                                      (gl:enable :blend :line-smooth :polygon-smooth)
                                      (gl:blend-func :src-alpha :one-minus-src-alpha)
                                      (gl:hint :line-smooth-hint :nicest)
                                      (gl:hint :polygon-smooth-hint :nicest)
                                      (gl:clear-color 0.0 1.0 0.0 1.0)
                                      (gl:clear :color-buffer :depth-buffer)
                                      (gl:flush))
                                    (s::with-environment env
                                      (s:with-identity-matrix
                                        (s::start-draw)
                                        (draw clock)
                                        (s::end-draw)))
                                        ;(gl:clear-color 0 (saw 10 (sc:time clock)) 0 1)
                                        ;(gl:clear :color-buffer-bit)
                                        (gtk:gtk-widget-queue-draw widget)
                                    )))
      (gtk:gtk-container-add box area)
      (gtk:gtk-container-add window box)
      (gtk:gtk-widget-show-all window))))
