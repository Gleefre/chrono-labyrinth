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

;;; Directions and positions

(defun inverse-direction (direction)
  (ecase direction
    (:stay  :stay)
    (:up    :down)
    (:down  :up)
    (:right :left)
    (:left  :right)))

(defun timed-direction (direction time-flow)
  (ecase time-flow
    (:forwards direction)
    (:backwards (inverse-direction direction))))

(defun in-direction (position direction)
  (when position
    (destructuring-bind (x y) position
      (ecase direction
        (:stay)
        (:up    (incf y))
        (:down  (decf y))
        (:right (incf x))
        (:left  (decf x)))
      (list x y))))
