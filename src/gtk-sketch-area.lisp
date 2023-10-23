(in-package #:chrono-labyrinth)

(defun %%make-scancode (gtk-keycode)
  ;; FIXME
  (sdl2:scancode-key-to-value :scancode-unknown))

(defun %%make-value (gtk-keyval)
  ;; FIXME
  (autowrap:enum-value 'sdl2-ffi:sdl-key-code :unknown))

(defun %%make-mode (gtk-state)
  ;; FIXME
  0
  )

(defun %%make-keysym (gtk-keyval gtk-keycode gtk-state)
  (plus-c:c-let ((ks sdl2-ffi:sdl-keysym :calloc t))
    (setf (ks :scancode) (%%make-scancode gtk-keycode)
          (ks :sym) (%%make-value gtk-keyval)
          (ks :mod) (%%make-mod gtk-state))
    (ks plus-c:&)))

(defstruct (sketch-area (:conc-name nil) (:constructor %make-sketch-area))
  gl-area sketch)

(defun make-sketch-area (sketch-name &rest initargs &key &allow-other-keys)
  (let* ((area (gtk:make-gl-area))
         (sketch (apply #'make-instance sketch-name :no-window t initargs))
         (sketch-area (%make-sketch-area :gl-area area :sketch sketch))
         (lx 0)
         (ly 0)
         (bmask 0)
         (pressed (make-hash-table)))
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
    (gtk:connect area "resize"
                 (lambda (area w h)
                   (declare (ignore area))
                   (setf (sketch:sketch-width sketch) w
                         (sketch:sketch-height sketch) h)
                   (kit.sdl2:window-event sketch :size-changed nil w h)))
    (setf (gtk:widget-focusable-p area) t
          (gtk:widget-can-target-p area) t)
    (let ((controller (gtk:make-event-controller-key)))
      (gtk:connect controller "key-pressed"
                   (lambda (controller keyval keycode state)
                     (declare (ignore controller))
                     (kit.sdl2:keyboard-event sketch :keydown nil
                                              (prog1 (gethash keycode pressed)
                                                (setf (gethash keycode pressed) t))
                                              (%%make-keysym keyval keycode state))
                     ;; FIXME: Use imcontext?
                     (kit.sdl2:textinput-event sketch nil "")))
      (gtk:connect controller "key-released"
                   (lambda (controller keyval keycode state)
                     (declare (ignore controller))
                     (setf (gethash keycode pressed) nil)
                     (kit.sdl2:keyboard-event sketch :keydown nil nil (%%make-keysym keyval keycode state))))
      (gtk:widget-add-controller area controller))
    (let ((controller (gtk:make-event-controller-motion)))
      (gtk:connect controller "enter"
                   (lambda (controller x y)
                     (declare (ignore controller))
                     (prog1 (kit.sdl2:mousemotion-event sketch nil bmask x y (- x lx) (- y ly))
                       (setf lx x ly y))))
      (gtk:connect controller "motion"
                   (lambda (controller x y)
                     (declare (ignore controller))
                     (prog1 (kit.sdl2:mousemotion-event sketch nil bmask x y (- x lx) (- y ly))
                       (setf lx x ly y))))
      (gtk:widget-add-controller area controller))
    (let ((controller (gtk:make-gesture-click)))
      (setf (gtk:gesture-single-button controller) 0)
      (gtk:connect controller "pressed"
                   (lambda (controller num x y)
                     (declare (ignore num))
                     (let ((button (gtk:gesture-single-current-button controller)))
                       (setf (ldb (byte 1 (1- button)) bmask) 1)
                       (kit.sdl2:mousebutton-event sketch :mousebuttondown nil button x y))))
      (gtk:connect controller "released"
                   (lambda (controller num x y)
                     (declare (ignore num))
                     (let ((button (gtk:gesture-single-current-button controller)))
                       (setf (ldb (byte 1 (1- button)) bmask) 0)
                       (kit.sdl2:mousebutton-event sketch :mousebuttonup nil button x y))))
      (gtk:widget-add-controller area controller))
    sketch-area))
