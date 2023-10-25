(in-package #:chrono-labyrinth)

(defconstant +tile-side+ 48)
(defconstant +tile-count+ 29)

(defclass tile ()
  ((id :accessor id :initarg :id)))

(defparameter +tiles-count-v+ 100)
(defparameter +tiles-count-h+ 100)
(defparameter +tiles-per-row+ 10)

(defparameter *tiles* (make-array (list +tiles-count-v+ +tiles-count-h+)))
(defparameter *editor-tile* (make-instance 'tile :id 0))

;; TODO: rename to game/load-tiles
(defun load-tiles2 ()
  (loop for x from 0 below +tiles-count-h+
	do (loop for y from 0 below +tiles-count-v+
		 do (setf (aref *tiles* x y) (make-instance 'tile :id 3)))))

(defun draw-tile (tile-id->texture tile x y)
  (s:image (gethash (id tile) tile-id->texture) x y))

(s:defsketch tile-test
    ((s:title "tile")
     ;; NOTE: Because this code is executed before sketch initialization
     ;; there is a memory fault when the sdl2-image:load-image and the gl:gen-texture
     ;; go one after another.
     (rsc nil)
     (tile-id->texture nil))

  (unless rsc
    (setf rsc (s:load-resource
               (data-path "textures/tiles.png")))
    (setf tile-id->texture (make-hash-table))
    (dotimes (id +tile-count+)
      (multiple-value-bind (row column) (floor id +tiles-per-row+)
	(let ((source-x (* column +tile-side+))
	      (source-y (* row +tile-side+)))
	  (setf (gethash id tile-id->texture)
		(s:crop rsc
			source-x source-y
			+tile-side+ +tile-side+))))))

  (s:with-pen (s:make-pen)
    (loop for x from 0 below +tiles-count-h+
	  do (loop for y from 0 below +tiles-count-v+
		   do (draw-tile tile-id->texture
				 (aref *tiles* x y)
				 (* x +tile-side+)
				 (* y +tile-side+))))))

(defmethod kit.sdl2:mousebutton-event ((sketch tile-test) st ts but x y)
  (declare (ignore ts))
  (when (and (eq :mousebuttondown st)
             (eq but 1))
    (let ((x (floor x +tile-side+))
          (y (floor y +tile-side+)))
      (when (and (<= 0 x (1- +tiles-count-h+))
                 (<= 0 y (1- +tiles-count-v+)))
        (setf (aref *tiles* x y) *editor-tile*)))))

(defparameter *sketch-name-for-area* 'tile-test)
(defparameter *quit-on-close* t)

;; TODO: rename to editor/load-tiles.
(defun load-tiles (list-box)
  (let* ((tiles (gdk-pixbuf2:make-pixbuf
		 :filename (data-path "textures/tiles.png")))
	 (width (gdk-pixbuf2:pixbuf-width tiles))
	 (height (gdk-pixbuf2:pixbuf-height tiles))
	 (tile-count 0))
    (dotimes (y (/ height +tile-side+))
      (dotimes (x (/ width +tile-side+))
	(when (< tile-count 29)
	  (let ((image (gtk:make-image
			:pixbuf (gdk-pixbuf2:pixbuf-new-subpixbuf
				 tiles
				 (* x +tile-side+)
				 (* y +tile-side+)
				 +tile-side+
				 +tile-side+))))
	    (let ((box (gtk:make-box :orientation gtk:+orientation-vertical+
				     :spacing 5))
		  (label (gtk:make-label :str (case tile-count
						(0 "Empty : 0")
						(1 "White : 1")
						(t (format nil "~a" tile-count))))))
	      (gtk:box-append box image)
	      (gtk:box-append box label)

	      (setf (gtk:widget-margin-start box) 5)
	      (setf (gtk:widget-margin-top box) 5)
	      (setf (gtk:widget-margin-end box) 5)
	      (setf (gtk:widget-margin-bottom box) 5)

	      (gtk:list-box-insert list-box box tile-count)
	      (incf tile-count))))))))

(gtk:define-application (:name simple-counter
                         :id "chrono.maze")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))

    (load-tiles2)

    (setf (gtk:window-title window) "Chrono Maze")
    (setf (gtk:window-default-size window) '(900 800))

    (let ((box (gtk:make-box :orientation gtk:+orientation-horizontal+
                             :spacing 4)))

      (let ((scrolled-window (gtk:make-scrolled-window))
	    (list-box (gtk:make-list-box)))
	(load-tiles list-box)
        (gtk:connect list-box "row-activated"
                     (lambda (self row)
                       (setf *editor-tile*
                             (make-instance 'tile :id (gtk:list-box-row-index row)))))
	(setf (gtk:scrolled-window-child scrolled-window) list-box)

	(setf (gtk:widget-hexpand-p scrolled-window) t)
	(gtk:box-append box scrolled-window))

      (let ((right-box (gtk:make-box :orientation gtk:+orientation-vertical+
				     :spacing 4)))

	(let* ((sketch-area (make-sketch-area *sketch-name-for-area*))
               (area (gl-area sketch-area)))
          (setf (gtk:widget-size-request area) '(800 800))
          (gtk:box-append right-box area))

	(let ((button (gtk:make-button :label "Exit")))
          (gtk:connect button "clicked" (lambda (button)
                                          (declare (ignore button))
                                          (gtk:window-destroy window)
                                          (when *quit-on-close*
                                            (uiop:quit))))
          (gtk:box-append right-box button))

	(gtk:box-append box right-box))

      (setf (gtk:window-child window) box))

    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))
