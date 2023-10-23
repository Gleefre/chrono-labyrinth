(in-package #:chrono-labyrinth)

(s:defsketch tile-test
    ((s:title "tile")
     ;; NOTE: Because this code is executed before sketch initialization
     ;; there is a memory fault when the sdl2-image:load-image and the gl:gen-texture
     ;; go one after another.
     (rsc nil))
  (unless rsc
    (setf rsc (s:load-resource
               (data-path "textures/sprite_sheet.png"))))
  (s:image (s:crop rsc 32 32 32 32) 10 10)
  (s:image (s:crop rsc 32 0 32 32) 42 10))

(defparameter *sketch-name-for-area* 'tile-test)

(gtk:define-application (:name simple-counter
                         :id "chrono.maze")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))
    (setf (gtk:window-title window) "Chrono Maze")
    (setf (gtk:window-default-size window) '(800 900))
    (let ((box (gtk:make-box :orientation gtk:+orientation-vertical+
                             :spacing 4)))
      (let* ((sketch-area (make-sketch-area *sketch-name-for-area*))
             (area (gl-area sketch-area)))
        (setf (gtk:widget-size-request area) '(800 800))
        (gtk:box-append box area)
        (let ((button (gtk:make-button :label "Exit")))
          (gtk:connect button "clicked" (lambda (button)
                                          (declare (ignore button))
                                          (gtk:window-destroy window)
                                          #+sbcl (sb-ext:exit)))
          (setf (gtk:widget-hexpand-p button) t
                (gtk:widget-vexpand-p button) t)
          (gtk:box-append box button)))
      (setf (gtk:window-child window) box))
    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))
