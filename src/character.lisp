(in-package #:chrono-labyrinth)

(defun draw-character (w h dir)
  (let ((id (case dir
              (:front 0)
              (:back  1)
              (:left  2)
              (:right 3)
              (t 4))))
    (multiple-value-bind (y x) (floor id 4)
      (s:image (s:crop
                (s:load-resource (data-path "textures/character.png")
                                 :mag-filter :nearest
                                 :min-filter :linear
                                 :force-reload-p t)
                (* 64 x) (* 64 y) 64 64)
               0 0 w h))))

(s:defsketch character-test ((dir :front)
                             (cycle-clock))
  (when cycle-clock
    (setf dir (elt '(:front :left :back :right)
                   (mod (floor (sc:time cycle-clock)) 4))))
  (s:background (s:gray 0.85))
  (s:with-pen (s:make-pen)
    (s+:with-fit (64 64 s:width s:height)
      (draw-character 64 64 dir))))

(defmethod kit.sdl2:keyboard-event ((sketch character-test) st ts rep? keysym)
  (when (and (eq st :keydown)
             (not rep?))
    (with-slots (dir cycle-clock) sketch
      (let ((next-dir (case (sdl2:scancode keysym)
                        ((:scancode-a :scancode-left) :left)
                        ((:scancode-d :scancode-right) :right)
                        ((:scancode-w :scancode-up) :back)
                        ((:scancode-s :scancode-down) :front))))
        (if next-dir
            (setf dir next-dir)
            (case (sdl2:scancode keysym)
              (:scancode-equals (when cycle-clock (sc:accelerate cycle-clock 1.4)))
              (:scancode-minus  (when cycle-clock (sc:accelerate cycle-clock (/ 1.4))))
              (:scancode-space  (setf cycle-clock (if cycle-clock nil (sc:make-clock))))))))))

#+(or) (make-instance 'character-test :resizable t)
