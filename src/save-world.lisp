(in-package #:chrono-labyrinth)

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
