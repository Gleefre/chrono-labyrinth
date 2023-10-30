(in-package #:chrono-labyrinth)

(defclass tileset ()
  ((filename   :initarg :filename   :accessor tileset-filename)
   (tile-side  :initarg :tile-side  :accessor tileset-tile-side)
   (columns    :initarg :columns    :accessor tileset-columns)
   (count      :initarg :count      :accessor tileset-count      :initform -1)
   (mag-filter :initarg :mag-filter :accessor tileset-mag-filter :initform :nearest)
   (min-filter :initarg :min-filter :accessor tileset-min-filter :initform :linear)))

(defun load-tile (tileset id &key force-reload-p
                                  color
                                  (mag-filter (tileset-mag-filter tileset))
                                  (min-filter (tileset-min-filter tileset))
                  &aux (side (tileset-tile-side tileset))
                       (count (tileset-count tileset)))
  (assert (not (minusp count)))
  (unless (minusp count)
    (assert (< id count)))
  (multiple-value-bind (y x) (floor id (tileset-columns tileset))
    (s::colored-image
     (s:load-resource (data-path (tileset-filename tileset))
                      :x (* side x)
                      :y (* side y)
                      :w side
                      :h side
                      :mag-filter mag-filter
                      :min-filter min-filter
                      :force-reload-p force-reload-p)
     color)))

(defun tile (id &key (x 0)
                     (y 0)
                     force-reload-p
                     color
                     (tileset *tileset*)
                     (side (tileset-tile-side tileset))
                     (mag-filter (tileset-mag-filter tileset))
                     (min-filter (tileset-min-filter tileset)))
  (s:image (load-tile tileset id
                      :force-reload-p force-reload-p
                      :color color
                      :mag-filter mag-filter
                      :min-filter min-filter)
           x y side side))

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
