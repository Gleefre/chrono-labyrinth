(in-package #:chrono-labyrinth)

(defparameter *editor-tile* 0)

(defun draw-tile (tile-id point &optional color)
  (if (< tile-id 7)
      (tile tile-id
            :x (x point)
            :y (y point)
            :tileset +world-tileset+
            :color color
            :side *side*)
      (if (= tile-id 7)
          (tile 8
                :x (x point)
                :y (y point)
                :tileset +world-tileset+
                :color color
                :side *side*)
          (tile (- tile-id 8)
                :x (x point)
                :y (y point)
                :tileset +character-tileset+
                :color color
                :side *side*))))

(defun choosed-area (choose-xy x y)
  (if choose-xy
      (destructuring-bind (x* y*) choose-xy
        (when (> x x*) (rotatef x x*))
        (when (> y y*) (rotatef y y*))
        (let ((rect (list x y (- x* x) (- y* y))))
          (let ((x  (floor x  *side*))
                (y  (floor y  *side*))
                (x* (floor x* *side*))
                (y* (floor y* *side*)))
            (list (list x x* y y*)
                  rect))))
      (list (list 0 -1 0 -1)
            (list -1 -1 0 0))))

(defun screen->world (x y width height)
  (destructuring-bind (x y) (s+:fit-point x y
                                          (camera-view-port-width) (camera-view-port-height)
                                          width height)
    (let ((world-point (camera-screen-to-world (make-point :x x :y y))))
      (list (x world-point)
            (y world-point)))))

(s:defsketch tile-test ((s:title "tile")
                        (choose-xy nil)
                        (world (make-instance 'world)))
  (s:background s:+black+)
  (s+:with-fit ((camera-view-port-width) (camera-view-port-height) s:width s:height)
    (s:with-pen (s:make-pen)
      (let ((*tileset* +world-tileset+))
        (draw-world (camera-view-port-width) (camera-view-port-height) world))
      (destructuring-bind ((x x+ y y+) (x= y= w= h=))
          (apply #'choosed-area choose-xy (screen->world (or (s:in :mouse-x) 0) (or (s:in :mouse-y) 0)
                                                         s:width s:height))
        (loop for xt from x to x+
              do (loop for yt from y to y+
                       do (when (camera-object-is-visible? (make-rectangle :x (* xt *side*)
                                                                           :y (* yt *side*)
                                                                           :width *side*
                                                                           :height *side*))
                            (s+:with-color (s:+black+)
                              (s:rect (* xt *side*) (* yt *side*) *side* *side*))
                            (draw-tile *editor-tile*
                                       (camera-world-to-screen
                                        (make-point :x (* xt *side*)
                                                    :y (* yt *side*)))
                                       (s:rgb 1 1 1 0.9)))))
        (s+:with-color (s:+red+ :stroke)
          (s:rect x= y= w= h=))))))

(defun toggle (position type &rest initargs)
  (if (some (a:rcurry #'typep type) (objects-at position))
      (mapcar #'remove-object (remove-if-not (a:rcurry #'typep type) (objects-at position)))
      (add-to-world (apply #'make-instance type :position position initargs))))

(defmethod kit.sdl2:mousebutton-event ((sketch tile-test) st ts but x y)
  (declare (ignore ts))
  (let ((*world* (tile-test-world sketch)))
    (destructuring-bind (x y) (screen->world x y (s:sketch-width sketch) (s:sketch-height sketch))
      (when (eq but 1)
        (case st
          (:mousebuttondown
           (setf (tile-test-choose-xy sketch) (list x y)))
          (:mousebuttonup
           (destructuring-bind (x* y*) (tile-test-choose-xy sketch)
             (setf (tile-test-choose-xy sketch) nil)
             (when (> x x*) (rotatef x x*))
             (when (> y y*) (rotatef y y*))
             (let ((x  (floor x  *side*))
                   (y  (floor y  *side*))
                   (x* (floor x* *side*))
                   (y* (floor y* *side*)))
               (loop for x from x to x*
                     do (loop for y from y to y*
                              do (case *editor-tile*
                                   (0 (mapcar #'remove-object (objects-at (list x y))))
                                   (1 (toggle (list x y) 'box))
                                   (2 (toggle (list x y) 'semi-wall))
                                   (3 (toggle (list x y) 'hourglass))
                                   (4 (toggle (list x y) 'ground))
                                   (5 (toggle (list x y) 'game-block))
                                   (6 (toggle (list x y) 'wall))
                                   (7 (toggle (list x y) 'level-exit))
                                   (8 (toggle (list x y) 'player)))))))))))))

(defmethod kit.sdl2:keyboard-event ((sketch tile-test) st ts repeat-p keysym)
  (when (eq st :keydown)
    (cond
      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
       (camera-move (make-point :y -32.0)))

      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
       (camera-move (make-point :y 32.0)))

      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
       (camera-move (make-point :x -32.0)))

      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
       (camera-move (make-point :x 32.0))))))

(defmethod kit.sdl2:window-event :after ((sketch tile-test) (type (eql :size-changed)) ts w h)
  (setf (camera-view-port-width) w)
  (setf (camera-view-port-height) h))

(defparameter *sketch-name-for-area* 'tile-test)
(defparameter *quit-on-close* nil)

(defun load-modes (list-box &aux (tile-count -1))
  (labels ((tile-pixbuf (tileset id &aux (side (tileset-tile-side tileset)))
             (multiple-value-bind (y x) (floor id (tileset-columns tileset))
               (gdk-pixbuf2:pixbuf-new-subpixbuf
                (gdk-pixbuf2:make-pixbuf
                 :filename (data-path (tileset-filename tileset)))
                (* x side)
                (* y side)
                side
                side)))
           (mode (name tileset id)
             (let ((image (gtk:make-image :pixbuf (tile-pixbuf tileset id)))
                   (box (gtk:make-box :orientation gtk:+orientation-vertical+
                                      :spacing 5))
                   (label (gtk:make-label :str name)))
               (gtk:box-append box image)
               (gtk:box-append box label)

               (setf (gtk:widget-margin-start box) 5)
               (setf (gtk:widget-margin-top box) 5)
               (setf (gtk:widget-margin-end box) 5)
               (setf (gtk:widget-margin-bottom box) 5)

               (gtk:list-box-insert list-box box (incf tile-count)))))
    (mapcar (lambda (name id) (mode name +world-tileset+ id))
            '("Empty" "Box" "Semi Wall" "Hourglass" "Ground" "Moving block" "Wall" "Exit")
            '( 0       1     2           3           4        5              6      8))
    (mode "Player" +character-tileset+ 0)))

#+darwin
(defmethod kit.sdl2:mousebutton-event :around ((sketch tile-test) st ts but x y)
  (let ((m 2))
    (call-next-method sketch st ts but (* x m) (* y m))))

#+darwin
(defmethod kit.sdl2:mousemotion-event :around ((sketch tile-test) ts bm x y xrel yrel)
  (let ((m 2))
    (call-next-method sketch ts bm (* x m) (* y m) (* xrel m) (* yrel m))))

(gtk:define-application (:name simple-counter
                         :id "chrono.maze")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))
    (setf (gtk:window-title window) "Chrono Maze")
    (setf (gtk:window-default-size window) '(900 800))

    (setf (camera-world-rectangle) (make-rectangle))

    (let ((box (gtk:make-box :orientation gtk:+orientation-horizontal+
                             :spacing 4)))
      (let ((scrolled-window (gtk:make-scrolled-window))
            (list-box (gtk:make-list-box)))
        (load-modes list-box)
        (gtk:connect list-box "row-activated"
                     (lambda (self row)
                       (declare (ignore self))
                       (setf *editor-tile* (gtk:list-box-row-index row))))
        (setf (gtk:scrolled-window-child scrolled-window) list-box)

        (setf (gtk:widget-hexpand-p scrolled-window) t)
        (gtk:box-append box scrolled-window))

      (let ((right-box (gtk:make-box :orientation gtk:+orientation-vertical+
                                     :spacing 4)))

        (let* ((sketch-area (make-sketch-area *sketch-name-for-area*))
               (area (gl-area sketch-area)))
          (setf (gtk:widget-size-request area) '(800 800))
          (gtk:box-append right-box area)

          (let ((bottom-right-box (gtk:make-box :orientation gtk:+orientation-horizontal+
                                                :spacing 4))
                (exit-btn (gtk:make-button :label "Close"))
                (save-btn (gtk:make-button :label "Save"))
                (load-btn (gtk:make-button :label "Load")))

            (gtk:connect exit-btn "clicked" (lambda (button)
                                              (declare (ignore button))
                                              (gtk:window-destroy window)
                                              (when *quit-on-close*
                                                (uiop:quit))))

            (gtk:connect save-btn "clicked"
                         (lambda (button)
                           (declare (ignore button))
                           (save-world (tile-test-world (sketch sketch-area)) "map")))

            (gtk:connect load-btn "clicked"
                         (lambda (button)
                           (declare (ignore button))
                           (setf (tile-test-world (sketch sketch-area)) (load-world "map"))))

            (gtk:box-append bottom-right-box save-btn)
            (gtk:box-append bottom-right-box load-btn)
            (gtk:box-append bottom-right-box exit-btn)

            (gtk:box-append right-box bottom-right-box)))

        (gtk:box-append box right-box))

      (setf (gtk:window-child window) box))

    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))
