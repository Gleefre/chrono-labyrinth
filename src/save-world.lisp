(in-package #:chrono-labyrinth)

;;; Copying

(defmethod copy-game-object (object)
  (list->object (object->list object)))

;;; Converting to list

(defmethod object->list (object)
  (list* (object->tag object)
         (object->untagged-list object)))

(defgeneric object->tag (object))  ; defined below

(defgeneric object->untagged-list (object)
  (:method-combination append))

(defmethod object->untagged-list append ((object game-object))
  (list :position (copy-list (object-position object))))

(defmethod object->untagged-list append ((object game-block))
  (list :offset (block-offset object)
        :on-cancel (block-on-cancel object)
        :program (coerce (block-program object) 'list)))

(defmethod object->untagged-list append ((player player))
  (list :action (player-action player)))

(defmethod object->untagged-list append ((hourglass hourglass))
  (list :charged (hourglass-charged hourglass)))

(defmethod object->untagged-list append ((world world))
  (list :time-flow (world-time-flow world)
        :objects (mapcar #'object->list (reverse (world-objects world)))))

;;; Converting from list

(defun list->object (list)
  (apply #'tag-list->object list))

(defmethod tag-list->object (tag &rest args)
  (apply #'make-instance (tag->class tag)
         (loop for (name thing) on args by #'cddr
               collect name collect (field->initarg name thing))))

(defmethod tag->class (tag) 'game-object)  ; defined below

(defmethod field->initarg (name thing) thing)

(defmethod field->initarg ((name (eql :position)) position)
  (copy-list position))

(defmethod field->initarg ((name (eql :program)) program)
  (coerce program 'vector))

;;; Trivial methods for tag->class and object->tag

(macrolet ((defmethods (&rest classes)
               `(progn
                  ,@(loop for object-class in classes
                          for tag = (a:make-keyword object-class)
                          collect `(defmethod object->tag ((object ,object-class)) ,tag)
                          collect `(defmethod tag->class ((tag (eql ,tag))) ',object-class)))))
  (defmethods
    game-block
    player
    box
    ground
    wall
    semi-wall
    hourglass
    level-exit
    world))

;; world
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
