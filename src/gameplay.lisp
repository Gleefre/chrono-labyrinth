(in-package #:chrono-labyrinth)

(defmethod move :before ((game-block game-block) action &optional (world *world*))
  (when (or (eq (block-on-cancel game-block) :continue)
            (eq (primary-action game-block (world-time-flow world))
                action))
    (ecase (world-time-flow world)
      (:forwards  (incf (block-offset game-block)))
      (:backwards (decf (block-offset game-block))))))

(defmethod deadp ((game-object game-object)) nil)

(defmethod deadp ((box box))
  (some (a:rcurry #'typep '(or game-block player wall))
        (objects-at (object-position box))))

(defmethod deadp ((player player))
  (some (a:rcurry #'typep '(or game-block wall))
        (objects-at (object-position player))))

(defun update-world (world &aux (time-flow (world-time-flow world)))
  (let ((*world* world))
    (with-actions-cached ()
      (dolist (object (world-objects world))
        ;; Calculating actions so that they are cached.
        ;; This is also needed for MOVE to work on game-blocks.
        (primary-action object time-flow)
        (action object time-flow))
      (dolist (object (world-objects world))
        (move object (action object time-flow)))
      (dolist (object (world-objects world))
        (when (deadp object)
          (remove-object object)))
      (cleanup-world)
      (when (eq :backwards (world-time-flow world))
        (decf (world-backwards-charges world)))
      (a:when-let* ((player (object-by-name :player))
                    (player-position (object-position player)))
        (loop for h? in (statics-at player-position)
              when (and (typep h? 'hourglass)
                        (hourglass-charged h?))
                do (setf (world-time-flow world) :backwards
                         (world-backwards-charges world) 5
                         (hourglass-charged h?) nil)))
      (unless (plusp (world-backwards-charges world))
        (setf (world-time-flow world) :forwards))))
  world)

(defun next-world (world)
  (update-world (copy-game-object world)))

(defun lose? (world)
  (not (object-by-name :player world)))

(defun win? (world)
  (a::when-let* ((player (object-by-name :player world))
                 (exit (object-by-name :exit world))
                 (player-pos (object-position player))
                 (exit-pos (object-position exit)))
    (equal player-pos exit-pos)))

(defclass game ()
  ((history :initform () :initarg :history :accessor history)
   (redo-history :initform () :accessor redo-history)
   (state :initform :menu :initarg :state :accessor state)
   (level :initform -1 :initarg :level :accessor level)))

(defun flevel (num)
  (data-path (format nil "map/~a.sexp" num)))

(defun load-level (id game)
  (typecase id
    (number (setf (level game) id
                  (history game) (list (load-world (flevel id)))
                  (redo-history game) nil
                  (state game) :level))
    (world (setf (level game) -1
                 (history game) (list (copy-game-object id))
                 (redo-history game) nil
                 (state game) :level))
    (string (setf (level game) -1
                  (history game) (list (load-world (data-path id)))
                  (redo-history game) nil
                  (state game) :level))
    (pathname (setf (level game) -1
                    (history game) (list (load-world id))
                    (redo-history game) nil
                    (state game) :level)))
  (unless (typep (car (history game)) 'world)
    (load-menu game)))

(defun reset-level (game)
  (setf (redo-history game) nil
        (history game) (last (history game))))

(defun load-menu (game)
  (setf (level game) -1
        (history game) nil
        (redo-history game) nil
        (state game) :menu))

(defun make-game ()
  (make-instance 'game))

(defun next-level (game)
  (load-level (1+ (level game)) game))
