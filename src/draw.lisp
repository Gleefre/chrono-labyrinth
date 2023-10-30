(in-package #:chrono-labyrinth)

;;; Layers

(defgeneric object-layer (object)
  (:method ((thing game-block)) 3)
  (:method ((thing player)) 3)
  (:method ((thing box)) 3)
  (:method ((thing ground)) 1)
  (:method ((thing wall)) 2)
  (:method ((thing semi-wall)) 2)
  (:method ((thing hourglass)) 2)
  (:method ((thing level-exit)) 2))

(defun layered (objects)
  (flet ((layer (id)
           (remove-if-not (a:curry #'eql id) objects :key #'object-layer)))
    (append (layer 1) (layer 2) (layer 3))))

(defgeneric object->tile (object)
  (:method ((thing game-block)) 5)
  (:method ((thing box)) 1)
  (:method ((thing ground)) 4)
  (:method ((thing wall)) 6)
  (:method ((thing semi-wall)) 2)
  (:method ((thing level-exit)) 8)
  (:method ((thing hourglass))
    (if (hourglass-charged thing)
        3
        7))
  (:method ((thing player))
    (case (player-action thing)
      ((:stay :down) 0)
      ((:up) 1)
      ((:left) 2)
      ((:right) 3))))

(defparameter **tile-color** nil)

(defmethod draw-object ((object game-object))
  (tile (object->tile object) :side *side* :color **tile-color**))

(defmethod draw-object :around ((object player))
  (let ((*tileset* +character-tileset+))
    (call-next-method)))

(defmethod draw-object :around ((object game-object))
  (with-slots (position) object
    (when position
      (destructuring-bind (x y) position
        (let ((rx (* *side* x))
              (ry (* *side* y)))
          (when (camera-object-is-visible? (make-rectangle :x rx :y ry :width *side* :height *side*))
            (s:with-translate (rx ry)
              (call-next-method))))))))

(defun draw-world (width height &optional (world *world*))
  (let ((**tile-color** **tile-color**))
    (when (eq (world-time-flow world) :backwards)
      (setf **tile-color** s:+magenta+))
    (s:with-pen (s:make-pen)
      (let ((*tileset* +world-tileset+))
        (s+:with-fit ((camera-view-port-width) (camera-view-port-height) width height)
          (with-camera-view ()
            (map nil #'draw-object (layered (world-objects world))))))))
  (s:with-font (s:make-font :color s:+white+ :size (/ width 10))
    (when (eq (world-time-flow world) :backwards)
      (s:text (format nil "Time reversed, charges: ~A" (world-backwards-charges world))
              0
              0))))

;; TODO figure out sketch figures?
(defun draw-clock (clock w h &aux (time (sc:time clock)))
  #+(or)  ; debug border
  (s+:with-color (s:+red+ :stroke)
    (s:rect 0 0 w h))
  (s+:with-fit (200 200 w h)
    (s:with-translate (100 100)
      (s:with-current-matrix
        (dotimes (_ 24)
          (s:rect 70 -7 30 14)
          (s:rotate (/ 360 24))))
      (s:with-pen (s:make-pen :stroke s:+yellow+ :weight 15)
        (s:with-rotate ((* 360 (/ time 24)))
          (s:line 20 0 60 0))))))

(defun draw-game (w h)
  (s+:with-fit (800 800 (/ w 4) (/ h 2) 0 0 0 (/ h 2))
    (s:background s:+black+)
    (s:with-translate (200 200)
      (draw-clock *game-clock* 400 400))
    (s:with-font (s:make-font :size 80 :align :center :color s:+white+)
      (s:text "Chrono Labyrinth" 400 650))
    (s:with-font (s:make-font :color s:+white+ :size 80)
      (case (state *game*)
        (:menu (s:text "MENU" 0 0))
        (:level (s:text "LEVEL" 0 0))
        (:win (s:text "CLEARED" 0 0))
        (:death (s:text "DEAD" 0 0))))
    (s:with-font (s:make-font :size 70 :color s:+white+ :align :left)
      (case (state *game*)
        (:win (s:text (format nil "    the next level ]~%   to descent onto~%[ press SPACE ") 40 -280))
        (:death (s:text (format nil "   restart]~%[ press R to ") 40 -200)))))
  (s:translate (/ w 4) 0)
  (setf (camera-view-port-width) (* 8 *side*)
        (camera-view-port-height) (* 8 *side*))
  (if (eq :menu (state *game*))
      (draw-menu (/ w 2) h)
      (draw-world (/ w 2) h (car (history *game*)))))

(defparameter +menu-text+
  (format nil "Press L to load a custom level.~2%M - [un]mute the soundtrack.~%Q - menu~%R - restart the level~%Z / X - undo / redo;~%SPACE - skip / descend;~%Arrows, WASD - movement;~%Controls:~%"))

(defun draw-menu (w h)
  (s+:with-fit (400 800 w h)
    (s:with-font (s:make-font :size 30 :align :center :color s:+white+)
      (s:text +menu-text+ 200 400))
    (s:with-font (s:make-font :size 50 :align :center :color s:+white+)
      (s:text "Press SPACE to start." 200 200))))
