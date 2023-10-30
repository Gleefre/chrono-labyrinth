(in-package #:chrono-labyrinth)

;;; Actions that will be taken.

;;; TIME-FLOW is one of :FORWARDS or :BACKWARDS.
;;; PRIMARY-ACTION is "naive" estimate of what action will be
;;; taken. ACTION is what action will really be taken - it can be
;;; different from primary-action if the destination square has a wall
;;; on it for example.
(serapeum:eval-always
  (defgeneric primary-action (object time-flow))
  (defgeneric primary-actionp (object time-flow action))
  (defgeneric action (object time-flow))
  (defgeneric actionp (object time-flow action)))

;;; Caching

(cache-methods (with-actions-cached)
  (primary-action ((object game-object) time-flow))
  (primary-actionp ((object game-object) time-flow action))
  (action ((object game-object) time-flow))
  (actionp ((object game-object) time-flow action)))

;;; Default methods
;;; Note: primary-action and primary-actionp are recursive

(defmethod primary-action ((object game-object) time-flow)
  (loop for act in '(:up :down :right :left)
        when (primary-actionp object time-flow act)
          collect act into acts
        finally (if (= 1 (length acts))
                    (return (car acts))
                    (return :stay))))

(defmethod primary-actionp ((object game-object) time-flow action)
  (or (eq action :stay)
      (eq action (primary-action object time-flow))))

(defmethod actionp ((object game-object) time-flow action)
  (or (eq action :stay)
      (primary-actionp object time-flow action)))

(defmethod action ((object game-object) time-flow)
  (loop for act in '(:up :down :right :left)
        when (actionp object time-flow act)
          do (return act)
        finally (return :stay)))

;;; Actions for statics

(defmethod primary-action ((static static) time-flow) :stay)

;;; Actions for blocks

;; Utility, needed for actionp on player and game-block
(defun blocks-moving-at (pos time-flow)
  (loop for dir in '(:up :down :right :left)
        for dir* = (inverse-direction dir)
        for pos* = (in-direction pos dir*)
        for movable = (movable-object-at pos*)
        when movable
          count (primary-actionp movable time-flow dir)))

(defmethod primary-action :before ((game-block game-block) time-flow)
  (with-slots (program offset) game-block
    (setf offset (mod offset (length program)))))

(defmethod primary-action ((game-block game-block) time-flow)
  (with-slots (program offset) game-block
    (timed-direction (aref program offset) time-flow)))

(defmethod actionp ((game-block game-block) time-flow action)
  (or (eq action :stay)
      (and (primary-actionp game-block time-flow action)
           (let ((target (in-direction (object-position game-block)
                                       action)))
             (and (not (some (a:rcurry #'typep 'wall)
                             (statics-at target)))
                  (= 1 (blocks-moving-at target time-flow))
                  (let ((object-ahead (movable-object-at target)))
                    (or (not (typep object-ahead 'game-block))
                        (actionp object-ahead time-flow action))))))))

;;; Actions for player

;; Utility for player actions
(defun direct-pushp (player time-flow direction)
  (and (not (eq direction :stay))
       (let* ((pos (in-direction (object-position player)
                                 (inverse-direction direction)))
              (movable (movable-object-at pos)))
         (and (typep movable 'game-block)
              (actionp movable time-flow direction)))))

(defmethod primary-action ((player player) time-flow)
  (if (not (eq (player-action player) :stay))
      (timed-direction (player-action player) time-flow)
      (call-next-method)))

(defmethod primary-actionp ((player player) time-flow action)
  (or (eq action :stay)
      (eq action (player-action player))
      (direct-pushp player time-flow action)))

(defmethod actionp ((player player) time-flow action)
  (or (eq action :stay)
      (and (primary-actionp player time-flow action)
           (let ((target (in-direction (object-position player)
                                       action)))
             (and (not (some (a:rcurry #'typep '(or wall semi-wall))
                             (statics-at target)))
                  (/= 1 (blocks-moving-at target time-flow))
                  (let ((object-ahead (movable-object-at target)))
                    (or (not (typep object-ahead 'game-block))
                        (actionp object-ahead time-flow action))))))))

(defmethod action ((player player) time-flow)
  (with-slots (action) player
    (if (and (not (eq action :stay))
             (actionp player time-flow action))
        action
        (call-next-method))))

;;; Actions for boxes

(defmethod primary-actionp ((box box) time-flow action)
  (or (eq action :stay)
      (let* ((pos (in-direction (object-position box)
                                (inverse-direction action)))
             (object-behind (movable-object-at pos)))
        (and (typep object-behind 'movable)
             (actionp object-behind time-flow action)))))
