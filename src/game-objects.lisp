(in-package #:chrono-labyrinth)

;;; Objects

(defclass game-object ()
  ((position :initform nil :accessor object-position :initarg :position)
   (layer :initform 2 :accessor object-layer :initarg :layer)))

(defclass named-object ()
  ((name :initform nil :accessor object-name)))

(defclass movable (game-object) ())

(defclass static (game-object) ())

(defclass game-block (movable)
  ((program   :initform (vector :stay) :accessor block-program   :initarg :program)
   (offset    :initform 0              :accessor block-offset    :initarg :offset)
   (on-cancel :initform :repeat        :accessor block-on-cancel :initarg :on-cancel)))

(defclass player (movable named-object)
  ((action :initform :stay :accessor player-action)
   (name   :initform :player)))

(defclass box (movable) ())

(defclass wall (static) ())

(defclass semi-wall (static) ())

(defclass hourglass-1 (static) ())

(defclass hourglass-2 (static) ())

(defclass ground (static) ())

(defclass water (static) ())

(defclass empty (static) ())

(defclass world ()
  ((map       :initform (make-hash-table :test 'equal) :accessor world-map)
   (time-flow :initform :forwards                      :accessor world-time-flow :initarg :time-flow)
   (objects   :initform (list)                         :accessor world-objects)
   (table     :initform (make-hash-table :test 'eq)    :accessor world-table)))

;;; Interacting with world

(defun object-by-name (name &optional (world *world*))
  (gethash name (world-table world)))

(defun objects-at (position &optional (world *world*))
  (gethash position (world-map world)))

(defun movable-object-at (position &optional (world *world*))
  (find-if (a:rcurry #'typep 'movable)
           (objects-at position world)))

(defun statics-at (position &optional (world *world*))
  (remove-if-not (a:rcurry #'typep 'static)
                 (objects-at position world)))

(defmethod add-to-world ((object game-object) &optional (world *world*))
  (a:when-let ((pos (object-position object)))
    (push object (gethash pos (world-map world))))
  (push object (world-objects world)))

(defmethod add-to-world :after ((object named-object) &optional (world *world*))
  (setf (gethash (object-name object) (world-table world))
        object))

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

(defmethod move ((object game-object) direction &optional (world *world*))
  (with-slots (position) object
    (when (and position (not (eq :stay direction)))
      (let ((next-position (in-direction position direction)))
        (a:removef (gethash position (world-map world)) object)
        (push object (gethash next-position (world-map world)))
        (setf position next-position)))))

(defgeneric remove-object (object &optional world)
  (:documentation "Removes object from this world. Doesn't remove from world-objects list, see CLEANUP-WORLD.")
  (:method-combination progn))

(defmethod remove-object progn ((object game-object) &optional (world *world*))
  (a:when-let ((pos (object-position object)))
    (a:removef (gethash pos (world-map world)) object)))

(defmethod remove-object progn ((object named-object) &optional (world *world*))
  (remhash (object-name object) (world-table world)))

(defun cleanup-world (&optional (world *world*))
  (setf (world-objects world)
        (remove-if-not #'object-position (world-objects world))))
