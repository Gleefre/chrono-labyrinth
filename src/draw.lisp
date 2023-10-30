(in-package #:chrono-labyrinth)

;;; Layers

(defgeneric object-layer (object)
  (:method ((thing game-block)) 3)
  (:method ((thing player)) 3)
  (:method ((thing box)) 3)
  (:method ((thing ground)) 1)
  (:method ((thing wall)) 1)
  (:method ((thing semi-wall)) 1)
  (:method ((thing hourglass)) 2)
  (:method ((thing level-exit)) 1))

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

(defmethod draw-object ((object game-object))
  (tile (object->tile object)))

(defmethod draw-object :around ((object player))
  (let ((*tileset* +character-tileset+))
    (call-next-method)))

(defmethod draw-object :around ((object game-object))
  (with-slots (position) object
    (when position
      (destructuring-bind (x y) position
        (s:with-translate ((* (tileset-tile-side +world-tileset+) x)
                           (* (tileset-tile-side +world-tileset+) y))
          (call-next-method))))))

(defun draw-world (width height camera &optional (world *world*))
  (declare (ignorable width height camera))
  (map nil #'draw-object (layered (world-objects world))))

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
  (s+:with-fit (800 800 w h)
    (s:background s:+black+)
    (s:with-translate (200 200)
      (draw-clock *game-clock* 400 400))
    (s:with-font (s:make-font :size 80 :align :center :color s:+white+)
      (s:text "Chrono Labyrinth" 400 650))))
