(in-package #:chrono-labyrinth)

(defstruct (sketch-area (:conc-name nil) (:constructor %make-sketch-area))
  gl-area sketch)

(defun make-sketch-area (sketch-name &rest initargs &key &allow-other-keys)
  (let* ((area (gtk:make-gl-area))
         (sketch (apply #'make-instance sketch-name :no-window t initargs))
         (sketch-area (%make-sketch-area :gl-area area :sketch sketch)))
    (gtk:connect area "render"
                 (lambda (area context)
                   (declare (ignore context))
                   (when (sketch::env-initialized-p (slot-value sketch 'sketch::%env))
                     (sketch::render sketch))
                   (gtk:idle-add (lambda () (gtk:widget-queue-draw area)))))
    (gtk:connect area "realize"
                 (lambda (area)
                   (sketch::initialize-sketch)
                   (gtk:gl-area-make-current area)
                   (sketch::initialize-environment sketch)
                   (sketch::initialize-gl sketch)))
    (gtk:connect area "unrealize"
                 (lambda (area)
                   (declare (ignore area))
                   (sketch::close-sketch sketch)))
    sketch-area))
