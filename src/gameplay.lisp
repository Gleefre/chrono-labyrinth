(in-package #:chrono-labyrinth)

(defmethod move ((game-block game-block) action &optional (world *world*))
  (when (or (eq (block-on-cancel game-block) :continue)
            (eq (primary-action game-block (world-time-flow world))
                action))
    (ecase (world-time-flow world)
      (:forwards  (incf (block-offset game-block)))
      (:backwards (decf (block-offset game-block))))))

(defmethod deadp ((game-object game-object)) nil)

(defmethod deadp ((box box))
  (some (alexandria:rcurry #'typep '(or fire game-block box player))
        (objects-at (object-position box))))

(defmethod deadp ((player player))
  (some (alexandria:rcurry #'typep '(or fire game-block))
        (objects-at (object-position player))))

(defun update-world (world &optional (time-flow (world-time-flow world) time-flow-p))
  (let ((*world* world))
    (when time-flow-p
      (setf (world-time-flow world) time-flow))
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
      (cleanup-world))))

(defun make-game ())
