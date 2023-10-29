(in-package #:chrono-labyrinth)

;;; FIXME: It would be better to use a gensym here,
;;; but that seems to break compiled code :/
(defmacro cache-methods ((with-cache-name &optional clear-cache-name) &body methods)
  (let ((cachep-var (a:symbolicate '#:%cache%* with-cache-name '#:-cachep*)
          #+nil (gensym "%*CACHEP*"))
        (key-var (gensym "KEY"))
        (cache-vars ()))
    `(serapeum:eval-always
       (progn
         (defvar ,cachep-var)
         ,@(loop for (name args) in methods
                 for cache-table-var = (a:symbolicate '#:%cache%* name '#:*)
                 #+nil (gensym (with-standard-io-syntax (format nil "%*~A-CACHE*" name)))
                 for key = `(list* ,@(mapcar #'car (mapcar #'a:ensure-list args)))
                 do (push cache-table-var cache-vars)
                 collect `(defvar ,cache-table-var)
                 collect `(defmethod ,name :around ,args
                            (if ,cachep-var
                                (let ((,key-var ,key))
                                  (or (gethash ,key-var ,cache-table-var)
                                      (setf (gethash ,key-var ,cache-table-var)
                                            (call-next-method))))
                                (call-next-method))))
         ,@(when clear-cache-name
             `((defun ,clear-cache-name ()
                 ,@(loop for var in cache-vars
                         collect `(clrhash ,var)))))
         (defmacro ,with-cache-name (&body body)
           `(let (,@',(loop for var in cache-vars
                            collect `(,var (make-hash-table :test 'equal)))
                  (,',cachep-var t))
              ,@body))))))

#+nil
(cache-methods (with-actions-cached clr-actions-cache)
  (primary-action ((object game-object) time-flow))
  (primary-actionp ((object game-object) time-flow action))
  (action ((object game-object) time-flow))
  (actionp ((object game-object) time-flow action)))

;; ~=>

#+nil
(progn
  (defvar %*possible-action-cache*)
  (defvar %*primary-direction-cache*)
  (defvar %*direction-cache*)
  (defvar %*directionp-cache*)
  (defparameter %*cache-direction* nil)

  (macrolet ((define-cache-method (name args &aux (special-var (a:symbolicate '#:%* name '#:-cache*)))
               `(defmethod ,name :around ((object game-object) ,@args)
                  (if %*cache-direction*
                      (let ((key (list* ,@args object)))
                        (or (gethash key ,special-var)
                            (setf (gethash key ,special-var)
                                  (call-next-method))))
                      (call-next-method)))))
    (define-cache-method primary-direction (time-flow))
    (define-cache-method direction (time-flow))
    (define-cache-method directionp (time-flow direction))
    (define-cache-method possible-action (action time-flow)))

  (defun clear-direction-cache ()
    (clrhash %*possible-action-cache*)
    (clrhash %*primary-direction-cache*)
    (clrhash %*direction-cache*)
    (clrhash %*directionp-cache*))

  (defmacro caching-direction (&body body)
    `(let ((%*possible-action-cache*   (make-hash-table :test 'equal))
           (%*primary-direction-cache* (make-hash-table :test 'equal))
           (%*direction-cache*         (make-hash-table :test 'equal))
           (%*directionp-cache*        (make-hash-table :test 'equal))
           (%*cache-direction* t))
       ,@body)))
