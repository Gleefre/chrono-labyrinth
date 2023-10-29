(in-package #:chrono-labyrinth)

;; data-path to get resource's path
(defparameter *data-location* "res/")

(let ((data-folder nil))
  (defun data-path (relative-path)
    (setf data-folder
          (or data-folder
              (if (member :deploy *features*)
                  (let ((deploy:*data-location* *data-location*))
                    (deploy:data-directory))
                  (asdf:system-relative-pathname "chrono-labyrinth" *data-location*))))
    (uiop:native-namestring
     (merge-pathnames relative-path data-folder))))

;; Change default font
(let ((font))
  (defun s::make-default-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path "font/PromptFont.ttf"))
                                :color s:+black+
                                :size 18)))))

(let ((font))
  (defun s::make-error-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path "font/PromptFont.ttf"))
                                :color s:+black+
                                :size 16)))))

;; SDL_GetPerformanceCounter based clock
(let ((frequency (sdl2:get-performance-frequency)))
  (defun sdl-performance-counter ()
    (/ (sdl2:get-performance-counter)
       frequency)))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))
