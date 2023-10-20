(in-package #:chrono-labyrinth)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (gobject:register-object-type "GtkGLArea" 'gtk-gl-area))

(gobject:define-g-object-class "GtkGLArea" gtk-gl-area
    (:superclass gtk:gtk-widget
     :export t
     :interfaces ("GtkBuildable")
     :type-initializer "gtk_gl_area_get_type")
    ())

(declaim (inline gtk-gl-area-new))

(defun gtk-gl-area-new ()
  (make-instance 'gtk-gl-area))

(cffi:defcfun ("gtk_gl_area_set_required_version" gtk-gl-area-set-required-version) :void
  (area (gobject:g-object gtk-gl-area))
  (major :int)
  (minor :int))

(cffi:defcfun ("gtk_gl_area_get_context" gtk-gl-area-get-context) (gobject:g-object gdk-gl-context)
  (area (gobject:g-object gtk-gl-area)))

(cffi:defcfun ("gtk_gl_area_attach_buffers" gtk-gl-area-attach-buffers) :void
  (area (gobject:g-object gtk-gl-area)))

(cffi:defcfun ("gtk_gl_area_make_current" gtk-gl-area-make-current) :void
  (area (gobject:g-object gtk-gl-area)))

(cffi:defcstruct g-error
  (domain glib:g-quark)
  (code :int)
  (message (:string :free-from-foreign nil)))

(cffi:defcfun ("gtk_gl_area_get_error" gtk-gl-area-get-error) :pointer
  (area (gobject:g-object gtk-gl-area)))

(defun gtk-gl-area-configure (widget)
  (gtk-gl-area-make-current widget)
  t
  )

(defun gtk-gl-area-on-render (widget context)
  (format t "widget ~a~%" widget)
  ;; (gtk-gl-area-attach-buffers widget)
  (gl:clear-color 1 0 0 1)
  (gl:clear :color-buffer-bit))

(defun example-window-simple ()
  (gtk:within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk:gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g:g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
      ;; Show the window.
      (gtk:gtk-widget-show-all window))))

(defun gtk-demo-activate (application)
  (let ((window (make-instance 'gtk:gtk-application-window
                               :application application
                               :type :toplevel
                               :title "GTK Lisp Code Demos"
                               :default-width 1000
                               :default-height 800))
	(box (gtk:gtk-box-new :vertical 10))
	(label (gtk:gtk-label-new "Test"))
	(area (gtk-gl-area-new)))

    ;; (gtk-gl-area-set-required-version area 3 3)

    ;; (gtk:gtk-container-add box label)
    ;; (gtk:gtk-container-add window box)

    (gtk:gtk-container-add window area)

    (format t "~a~%" (gtk-gl-area-get-error area))
    (format t "~a~%" area)
    (format t "~a~%" (gtk-gl-area-get-context area))
    ;; (print (cffi:foreign-slot-value (gtk-gl-area-get-error area) 'g-error 'message))

    (gobject:g-signal-connect area "render" #'gtk-gl-area-on-render)

    (format t "~a~%" (gtk-gl-area-get-error area))

    (gobject:g-signal-connect window "destroy"
			      (lambda (widget)
				(declare (ignore widget))
				(let ((action (gio:g-action-map-lookup-action application "quit")))
				  (print action)
				  (gio:g-action-activate action))))
    (gtk:gtk-widget-show-all window)
    ;; (gtk-gl-area-configure area)
))

(defun gtk-demo-startup (application)
  ;; Add action "quit" to the application
  (let ((action (gio:g-simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (gobject:g-signal-connect action "activate"
		      (lambda (action parameter)
			(declare (ignore action parameter))
			;; Destroy all windows of the application
			(dolist (window (gtk:gtk-application-windows application))
			  (gtk:gtk-widget-destroy window))))
    ;; Add the action to action map of the application
    (gio:g-action-map-add-action application action)))

#+:darwin
(defun run-gtk-example ()
  ;; (bt:make-thread
  ;;  (lambda ()
  ;;    (swank:create-server :port 4005 :dont-close t)))

  ;; (unless (string= "GTK Lisp Demo" (glib:g-application-name))
  ;;   (setf (glib:g-application-name) "GTK Lisp Demo"))

  (let ((gtk-demo (make-instance 'gtk:gtk-application
                                 :application-id "gtk-example"
                                 :register-session t)))
    ;; Connect signal handlers to the application
    (gobject:g-signal-connect gtk-demo "activate" #'gtk-demo-activate)
    (gobject:g-signal-connect gtk-demo "startup" #'gtk-demo-startup)
    ;; Start the application
    (gio:g-application-run gtk-demo ()))
  ;; (gtk-demo:gtk-demo)
  )

;; (progn (ql:quickload :chrono-labyrinth) (chrono-labyrinth::run-gtk-example))

;; (sb-thread:interrupt-thread (car (sb-thread:list-all-threads)) #'gtk:leave-gtk-main)

;; (progn
;;   (ql:quickload :gtk-example)
;;   (gtk-example:example-hello-world))

;; (ql:quickload :gtk-demo)
;; (ql:quickload :sdl2)
;; (sdl2:make-this-thread-main
;;  (lambda ()
;;    (sb-int:with-float-traps-masked (:inexact)
;;      (gtk-demo:gtk-demo))))

;; (sdl2:make-this-thread-main
;;  (lambda ()
;;    (gtk-demo:gtk-demo)))
