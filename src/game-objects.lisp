(in-package #:chrono-labyrinth)

;;; Objects

(defclass game-object ()
  ((position :initform nil :accessor object-position :initarg :position)
   (layer :initform 2 :accessor object-layer :initarg :layer)))

(defclass named-object (game-object)
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

(defmethod remove-object ((object game-object) &optional (world *world*))
  (a:when-let ((pos (object-position object)))
    (a:removef (gethash pos (world-map world)) object)))

(defmethod remove-object ((object named-object) &optional (world *world*))
  (remhash (object-name object) (world-table world)))

(defun cleanup-world (&optional (world *world*))
  (setf (world-objects world)
        (remove-if-not #'object-position (world-objects world))))

;;; Copying, saving, loading.

(defgeneric copy-game-object (object))
(defgeneric object->list (object))
(defgeneric tag-list->object (tag &rest args))
(defun list->object (list)
  (apply #'tag-list->object list))

;; TODO: A limited version for the wall, ground, etc
(defmacro gen-base-functions (object-class)
  `(progn
     (defmethod copy-game-object ((object ,object-class))
       (make-instance ',object-class
                      :position (copy-list (object-position object))
		      :layer (object-layer object)))

     (defmethod object->list ((object ,object-class))
       (list ,(make-keyword object-class)
	     :position (copy-list (object-position object))
	     :layer (object-layer object)))

     (defmethod tag-list->object ((tag (eql ,(make-keyword object-class))) &key position layer)
       (make-instance ',object-class
		      :position (copy-list position)
		      :layer layer))))

(gen-base-functions box)
(gen-base-functions wall)
(gen-base-functions semi-wall)
(gen-base-functions hourglass-1)
(gen-base-functions hourglass-2)
(gen-base-functions ground)
(gen-base-functions water)
(gen-base-functions empty)

;; game-block
(defmethod copy-game-object ((object game-block))
  (make-instance 'game-block
                 :position (copy-list (object-position object))
                 :offset (block-offset object)
                 :on-cancel (block-on-cancel object)
                 :program (a:copy-array (block-program object))))

(defmethod object->list ((object game-block))
  (list :game-block
        :position (copy-list (object-position object))
        :offset (block-offset object)
        :on-cancel (block-on-cancel object)
        :program (coerce (block-program object) 'list)))

(defmethod tag-list->object ((tag (eql :game-block)) &key position offset on-cancel program)
  (make-instance 'game-block
                 :position (copy-list position)
                 :offset offset
                 :on-cancel on-cancel
                 :program (coerce program 'vector)))

;; player
(defmethod copy-game-object ((object player))
  (make-instance 'player
                 :position (copy-list (object-position object))
                 :action (player-action object)))

(defmethod object->list ((object player))
  (list :player
        :position (copy-list (object-position object))
        :action (player-action object)))

(defmethod tag-list->object ((tag (eql :player)) &key position action)
  (make-instance 'player
                 :position (copy-list position)
                 :action action))

;; world
(defmethod copy-game-object ((object world))
  (let ((copy (make-instance 'world :time-flow (world-time-flow object))))
    (dolist (world-object (reverse (world-objects object)) copy)
      (add-to-world (copy-game-object world-object) copy))))

(defmethod object->list ((object world))
  (list :world
        :time-flow (world-time-flow object)
        :objects (mapcar #'object->list (reverse (world-objects object)))))

(defmethod tag-list->object ((tag (eql :world)) &key objects time-flow)
  (let ((world (make-instance 'world :time-flow time-flow)))
    (dolist (world-object (mapcar #'list->object objects) world)
      (add-to-world world-object world))))

;;; Saving to files

(defun save-world (world filename &key (if-exists :error))
  (a:with-output-to-file (out filename :if-exists if-exists)
    (with-standard-io-syntax
      (let ((*print-case* :downcase)
            (*print-pretty* t)
            (*print-readably* t)
            (*print-right-margin* 50))
        (print (object->list world) out)))))

(defun load-world (filename)
  (block nil
    (a:with-input-from-file (in filename :if-does-not-exist nil)
      (unless (streamp in)
        (return nil))
      (with-standard-io-syntax
        (let ((*read-eval* nil)) ; but setting this to T would allow to "hack" the game via worlds.
          (let ((world-list (read in nil nil)))
            (when (or (null world-list)
                      (not (eq :world (car world-list))))
              (return nil))
            (list->object world-list)))))))
