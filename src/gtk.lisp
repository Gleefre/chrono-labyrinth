(in-package #:chrono-labyrinth)

(defun saw (len x)
  (abs
   (- (/ (mod x len) (/ len 2))
      1)))

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
                                (lambda (widget context)
                                  (declare (ignore context))
                                  (gl:clear-color 0 (saw 10 (sc:time clock)) 0 1)
                                  (gl:clear :color-buffer-bit)
                                  (gtk:gtk-widget-queue-draw widget)))
      (gtk:gtk-container-add box area)
      (gtk:gtk-container-add window box)
      (gtk:gtk-widget-show-all window))))
