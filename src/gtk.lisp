(in-package #:chrono-labyrinth)

(defconstant +tile-side+ 48)
(defconstant +tile-count+ 29)

(defparameter +tiles-count-v+ 15)
(defparameter +tiles-count-h+ 15)
(defparameter +tiles-per-row+ 10)

(defparameter *tiles* (make-array (list +tiles-count-v+ +tiles-count-h+) :initial-element 3))
(defparameter *editor-tile* 0)

(defun draw-tile (tile-id &optional color)
  (multiple-value-bind (y x) (floor tile-id +tiles-per-row+)
    (s:image (s:colored-image
              (s:load-resource
               (data-path "textures/tiles.png")
               :x (* x +tile-side+)
               :y (* y +tile-side+)
               :w +tile-side+
               :h +tile-side+)
              color)
             0
             0)))

(defun choosed-area (choose-xy x y)
  (if choose-xy
      (destructuring-bind (x* y*) choose-xy
        (when (> x x*) (rotatef x x*))
        (when (> y y*) (rotatef y y*))
        (let ((rect (list x y (- x* x) (- y* y))))
          (let ((x  (floor x  +tile-side+))
                (y  (floor y  +tile-side+))
                (x* (floor x* +tile-side+))
                (y* (floor y* +tile-side+)))
            (setf x  (alexandria:clamp x  0 (1- +tiles-count-h+))
                  x* (alexandria:clamp x* 0 (1- +tiles-count-h+))
                  y  (alexandria:clamp y  0 (1- +tiles-count-v+))
                  y* (alexandria:clamp y* 0 (1- +tiles-count-v+)))
            (list (list x x* y y*)
                  rect))))
      (list (list -1 -1 -1 -1)
            (list -1 -1 -1 -1))))

(s:defsketch tile-test ((s:title "tile")
                        (choose-xy nil))
  (s:background s:+black+)
  (s+:with-fit ((* +tiles-count-h+ +tile-side+)
                (* +tiles-count-v+ +tile-side+)
                s:width s:height)
    (s:with-pen (s:make-pen)
      (destructuring-bind ((x x+ y y+) (x= y= w= h=))
          (apply #'choosed-area choose-xy
                 (s+:fit-point (or (s:in :mouse-x) 0) (or (s:in :mouse-y) 0)
                               (* +tiles-count-h+ +tile-side+)
                               (* +tiles-count-v+ +tile-side+)
                               s:width s:height))
        (loop for xt from 0 below +tiles-count-h+
              do (loop for yt from 0 below +tiles-count-v+
                       do (s:with-translate ((* xt +tile-side+) (* yt +tile-side+))
                            (if (and (<= x xt x+)
                                     (<= y yt y+))
                                (draw-tile *editor-tile* (s:rgb 1 1 1 0.9))
                                (draw-tile (aref *tiles* xt yt))))))
        (s+:with-color (s:+red+ :stroke)
          (s:rect x= y= w= h=))))))

(defmethod kit.sdl2:mousebutton-event ((sketch tile-test) st ts but x y)
  (declare (ignore ts))
  (destructuring-bind (x y) (s+:fit-point x y
                                          (* +tiles-count-h+ +tile-side+) (* +tiles-count-v+ +tile-side+)
                                          (s:sketch-width sketch) (s:sketch-height sketch))
    (when (eq but 1)
      (case st
        (:mousebuttondown
         (setf (tile-test-choose-xy sketch) (list x y)))
        (:mousebuttonup
         (destructuring-bind (x* y*) (tile-test-choose-xy sketch)
           (setf (tile-test-choose-xy sketch) nil)
           (when (> x x*) (rotatef x x*))
           (when (> y y*) (rotatef y y*))
           (let ((x  (floor x  +tile-side+))
                 (y  (floor y  +tile-side+))
                 (x* (floor x* +tile-side+))
                 (y* (floor y* +tile-side+)))
             (setf x  (alexandria:clamp x  0 (1- +tiles-count-h+))
                   x* (alexandria:clamp x* 0 (1- +tiles-count-h+))
                   y  (alexandria:clamp y  0 (1- +tiles-count-v+))
                   y* (alexandria:clamp y* 0 (1- +tiles-count-v+)))
             (loop for x from x to x*
                   do (loop for y from y to y*
                            do (setf (aref *tiles* x y) *editor-tile*))))))))))

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
    (setf (gtk:window-title window) "Chrono Maze")
    (setf (gtk:window-default-size window) '(900 800))

    (let ((box (gtk:make-box :orientation gtk:+orientation-horizontal+
                             :spacing 4)))
      (let ((scrolled-window (gtk:make-scrolled-window))
	    (list-box (gtk:make-list-box)))
	(load-tiles list-box)
        (gtk:connect list-box "row-activated"
                     (lambda (self row)
                       (setf *editor-tile* (gtk:list-box-row-index row))))
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
