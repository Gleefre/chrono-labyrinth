(in-package #:chrono-labyrinth)

;; TODO

(defparameter *soundtrack* NIL)
(defparameter *soundtrack-mute* T)

(defun create-soundtrack ()
  (setf *soundtrack*
        (make-instance 'h:environment :sets `((:normal ,(probe-file (data-path "soundtrack.ogg")))))))

(defun play-soundtrack ()
  (setf *soundtrack-mute* NIL)
  (h:transition *soundtrack* :normal))

(defun mute-soundtrack ()
  (setf *soundtrack-mute* T)
  (h:transition *soundtrack* NIL))

(defun toggle-soundtrack ()
  (if *soundtrack-mute*
      (play-soundtrack)
      (mute-soundtrack)))

(defun music-init ()
  (unless h:*server*
    (h:maybe-start-simple-server :mixers '((:music m:basic-mixer) (:effect m:basic-mixer))
                                 :name "Waller")
    (create-soundtrack)))

(defun music-quit ()
  (when h:*server*
    (h:stop h:*server*)
    (setf h:*server* nil)))
