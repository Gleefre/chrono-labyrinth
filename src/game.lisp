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
  (when (and (eq :keydown state)
             (not rep?)
             (eq :scancode-m (sdl2:scancode keysym)))
    (toggle-soundtrack))

  (when (and (eq :keydown state)
             (eq :scancode-z (sdl2:scancode keysym))
             (not (eq :menu (state *game*))))
    (unless (null (cdr (history *game*)))
      (push (pop (history *game*))
            (redo-history *game*))))

  (when (and (eq :keydown state)
             (eq :scancode-x (sdl2:scancode keysym))
             (not (eq :menu (state *game*))))
    (unless (null (redo-history *game*))
      (push (pop (redo-history *game*))
            (history *game*))))

  (when (and (eq :keydown state)
             (not rep?)
             (eq :scancode-r (sdl2:scancode keysym))
             (not (eq :menu (state *game*))))
    (setf (history *game*) (last (history *game*))
          (redo-history *game*) nil))

  (when (and (eq state :keydown)
             (not rep?))
    (case (state *game*)
      (:level
       (case (sdl2:scancode keysym)
         ((:scancode-w :scancode-up)
          (let ((*world* (copy-game-object *world*)))
            (setf (player-action (object-by-name :player)) :up)
            (push (next-world *world*) (history *game*))
            (setf (redo-history *game*) nil)))
         ((:scancode-s :scancode-down)
          (let ((*world* (copy-game-object *world*)))
            (setf (player-action (object-by-name :player)) :down)
            (push (next-world *world*) (history *game*))
            (setf (redo-history *game*) nil)))
         ((:scancode-a :scancode-left)
          (let ((*world* (copy-game-object *world*)))
            (setf (player-action (object-by-name :player)) :left)
            (push (next-world *world*) (history *game*))
            (setf (redo-history *game*) nil)))
         ((:scancode-d :scancode-right)
          (let ((*world* (copy-game-object *world*)))
            (setf (player-action (object-by-name :player)) :right)
            (push (next-world *world*) (history *game*))
            (setf (redo-history *game*) nil)))
         ((:scancode-q :scancode-space)
          (let ((*world* (copy-game-object *world*)))
            (setf (player-action (object-by-name :player)) :stay)
            (push (next-world *world*) (history *game*))
            (setf (redo-history *game*) nil)))))
      (:death
       (case (sdl2:scancode keysym)
         ((:scancode-space)
          (reset-level *game*))))
      (:win
       (case (sdl2:scancode keysym)
         ((:scancode-space)
          (next-level *game*))))
      (:menu
       (case (sdl2:scancode keysym)
         ((:scancode-space)
          (load-level 0 *game*))
         ((:scancode-l)
          (handler-case
              (multiple-value-bind (file file-p)
                  (org.shirakumo.file-select:existing :multiple nil
                                                      :default (data-path "map/")
                                                      :filter '(("S-expressions" "sexp")))
                (when file-p
                  (load-level file *game*)))
            (org.shirakumo.file-select:no-backend-found ()
              (warn "No usable backend for file-select could be found!"))))))))

  (setf (state *game*)
        (cond ((null (history *game*))
               :menu)
              ((lose? (car (history *game*)))
               :death)
              ((win? (car (history *game*)))
               :win)
              (t :level))))

(s:define-start-function (start) game-window
                         (:resizable t :width 800 :height 500)
  (:start (music-init) (play-soundtrack))
  (:on-close (w) (mute-soundtrack))
  (:quit (music-quit)))

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
