(in-package #:chrono-labyrinth)

;;; Tilesets

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

;;; Existing tilesets

(defparameter +character-tileset+
  (make-instance 'tileset
                 :filename "textures/character.png"
                 :tile-side 64
                 :columns 4
                 :count 4))

(defparameter +world-tileset+
  (make-instance 'tileset
                 :filename "textures/tiles-min.png"
                 :tile-side 32
                 :columns 4
                 :count 8))
