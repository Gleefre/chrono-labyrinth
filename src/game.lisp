(in-package #:chrono-labyrinth)

(s:defsketch game-window ((s:title "Chrono Labyrinth")
                          (game (make-game))
                          (clock (sc:make-clock :time-source #'sdl-performance-counter))
                          (s:y-axis :up))
  (let ((*game* game)
        (*game-clock* clock)
        (*game-window* s::*sketch*))
    (draw-game s:width s:height)))

(defmethod kit.sdl2:keyboard-event ((game game-window) state ts rep? keysym)
  (when (and (eq state :keydown)
             (not rep?))
    (if (not (lose? (car (history *game*))))
        (case (sdl2:scancode keysym)
          ((:scancode-w :scancode-up)
           (let ((*world* (copy-game-object *world*)))
             (setf (player-action (object-by-name :player)) :up)
             (push (next-world *world*) (history *game*))))
          ((:scancode-s :scancode-down)
           (let ((*world* (copy-game-object *world*)))
             (setf (player-action (object-by-name :player)) :down)
             (push (next-world *world*) (history *game*))))
          ((:scancode-a :scancode-left)
           (let ((*world* (copy-game-object *world*)))
             (setf (player-action (object-by-name :player)) :left)
             (push (next-world *world*) (history *game*))))
          ((:scancode-d :scancode-right)
           (let ((*world* (copy-game-object *world*)))
             (setf (player-action (object-by-name :player)) :right)
             (push (next-world *world*) (history *game*))))
          ((:scancode-z)
           (unless (null (cdr (history *game*)))
             (pop (history *game*)))))
        (case (sdl2:scancode keysym)
          ((:scancode-r)
           (setf (history *game*) (last (history *game*))))
          ((:scancode-z)
           (unless (null (cdr (history *game*)))
             (pop (history *game*)))))))
  (when (and rep? (eq :scancode-z (sdl2:scancode keysym)))
    (unless (null (cdr (history *game*)))
      (pop (history *game*)))))

(s:define-start-function (start) game-window
                         (:resizable t :width 800 :height 500))

(defmethod kit.sdl2:mousebutton-event :around ((window game-window) state ts button x y)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window))
    (let ((*world* (car (history *game*))))
      (call-next-method))))

(defmethod kit.sdl2:keyboard-event :around ((window game-window) state ts rep? keysym)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window))
    (let ((*world* (car (history *game*))))
      (call-next-method))))
