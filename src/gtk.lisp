(in-package #:chrono-labyrinth)

(defconstant +tile-side+ 32)
(defconstant +tile-count+ 8)

(defparameter +tiles-count-v+ 100)
(defparameter +tiles-count-h+ 100)
(defparameter +tiles-per-row+ 4)

(defparameter *layer-1* (make-array (list +tiles-count-v+ +tiles-count-h+) :initial-element 0))
(defparameter *layer-2* (make-array (list +tiles-count-v+ +tiles-count-h+) :initial-element 0))
(defparameter *editor-tile* 0)

(defun draw-tile (tile-id point &optional color)
  (multiple-value-bind (sy sx) (floor tile-id +tiles-per-row+)
    (s:image (s:crop (s:load-resource
		      (data-path "textures/tiles-min.png"))
		     (* sx +tile-side+)
		     (* sy +tile-side+)
		     +tile-side+
		     +tile-side+)
	     (x point)
	     (y point))))

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
            (setf x  (a:clamp x  0 (1- +tiles-count-h+))
                  x* (a:clamp x* 0 (1- +tiles-count-h+))
                  y  (a:clamp y  0 (1- +tiles-count-v+))
                  y* (a:clamp y* 0 (1- +tiles-count-v+)))
            (list (list x x* y y*)
                  rect))))
      (list (list -1 -1 -1 -1)
            (list -1 -1 -1 -1))))

(defun screen->world (x y width height)
  (destructuring-bind (x y) (s+:fit-point x y
                                          800 800
                                          width height)
    (let* ((world-point (camera-screen-to-world (make-point :x x :y y)))
	   (x (x world-point))
	   (y (y world-point)))
      (list x y))))

(defparameter *current-layer* 1)

(defun draw-layer (layer)
  (loop for xt from 0 below +tiles-count-h+
        do (loop for yt from 0 below +tiles-count-v+
                 do (when (camera-object-is-visible? (make-rectangle :x (* xt +tile-side+)
								     :y (* yt +tile-side+)
								     :width +tile-side+
								     :height +tile-side+))
		      (draw-tile (aref layer xt yt)
				 (camera-world-to-screen
				  (make-point :x (* xt +tile-side+)
					      :y (* yt +tile-side+))))))))

(defun draw-all-layers ()
  (draw-layer *layer-1*)
  (draw-layer *layer-2*))

(s:defsketch tile-test ((s:title "tile")
                        (choose-xy nil))
  (s:background s:+black+)
  (s+:with-fit (800 800 s:width s:height)
    (s:with-pen (s:make-pen)
      (if (zerop *current-layer*)
	  (draw-all-layers)
	  (destructuring-bind ((x x+ y y+) (x= y= w= h=))
              (apply #'choosed-area choose-xy (screen->world (or (s:in :mouse-x) 0) (or (s:in :mouse-y) 0)
							     s:width s:height))
	    (let ((layer (ecase *current-layer*
			   (1 *layer-1*)
			   (2 *layer-2*))))
	      (loop for xt from 0 below +tiles-count-h+
		    do (loop for yt from 0 below +tiles-count-v+
			     do (when (camera-object-is-visible? (make-rectangle :x (* xt +tile-side+)
										 :y (* yt +tile-side+)
										 :width +tile-side+
										 :height +tile-side+))
				  (if (and (<= x xt x+)
					   (<= y yt y+))
				      (draw-tile *editor-tile*
						 (camera-world-to-screen
						  (make-point :x (* xt +tile-side+)
							      :y (* yt +tile-side+)))
						 (s:rgb 1 1 1 0.9))
				      (draw-tile (aref layer xt yt)
						 (camera-world-to-screen
						  (make-point :x (* xt +tile-side+)
							      :y (* yt +tile-side+)))))))))
	    (s+:with-color (s:+red+ :stroke)
              (s:rect x= y= w= h=)))))))

(defmethod kit.sdl2:mousebutton-event ((sketch tile-test) st ts but x y)
  (declare (ignore ts))
  (destructuring-bind (x y) (screen->world x y (s:sketch-width sketch) (s:sketch-height sketch))
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
             (setf x  (a:clamp x  0 (1- +tiles-count-h+))
                   x* (a:clamp x* 0 (1- +tiles-count-h+))
                   y  (a:clamp y  0 (1- +tiles-count-v+))
                   y* (a:clamp y* 0 (1- +tiles-count-v+)))
	     (let ((layer (ecase *current-layer*
			    (1 *layer-1*)
			    (2 *layer-2*))))
               (loop for x from x to x*
                     do (loop for y from y to y*
                              do (setf (aref layer x y) *editor-tile*)))))))))))

(defmethod kit.sdl2:keyboard-event ((sketch tile-test) st ts repeat-p keysym)
  (when (eq st :keydown)
    (cond
      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
       (camera-move (make-point :y -8.0)))

      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
       (camera-move (make-point :y 8.0)))

      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
       (camera-move (make-point :x -8.0)))

      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
       (camera-move (make-point :x 8.0))))))

(defparameter *sketch-name-for-area* 'tile-test)
(defparameter *quit-on-close* t)

;; TODO: rename to editor/load-tiles.
(defun load-tiles (list-box)
  (let* ((tiles (gdk-pixbuf2:make-pixbuf
		 :filename (data-path "textures/tiles-min.png")))
	 (width (gdk-pixbuf2:pixbuf-width tiles))
	 (height (gdk-pixbuf2:pixbuf-height tiles))
	 (tile-count 0))
    (dotimes (y (/ height +tile-side+))
      (dotimes (x (/ width +tile-side+))
	(when (< tile-count +tile-count+)
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

#+darwin
(defmethod kit.sdl2:mousebutton-event :around ((sketch sketch::sketch) st ts but x y)
  (let ((m 2))
    (call-next-method sketch st ts but (* x m) (* y m)))
  #+()(call-next-method sketch st ts but (/ x 1.5) (/ y 1.5)))

#+darwin
(defmethod kit.sdl2:mousemotion-event :around ((sketch sketch::sketch) ts bm x y xrel yrel)
  (let ((m 2))
    (call-next-method sketch ts bm (* x m) (* y m) (* xrel m) (* yrel m)))
  #+()(call-next-method sketch ts bm (/ x 1.5) (/ y 1.5) (/ xrel 1.5) (/ yrel 1.5)))

(gtk:define-application (:name simple-counter
                         :id "chrono.maze")
  (gtk:define-main-window (window (gtk:make-application-window :application gtk:*application*))
    (setf (gtk:window-title window) "Chrono Maze")
    (setf (gtk:window-default-size window) '(900 800))

    (setf (camera-world-rectangle) (make-rectangle :width (* +tiles-count-h+ +tile-side+)
						   :height (* +tiles-count-v+ +tile-side+)))

    (let ((box (gtk:make-box :orientation gtk:+orientation-horizontal+
                             :spacing 4)))
      (let ((scrolled-window (gtk:make-scrolled-window))
	    (list-box (gtk:make-list-box)))
	(load-tiles list-box)
        (gtk:connect list-box "row-activated"
                     (lambda (self row)
                       (declare (ignore self))
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

	(let ((bottom-right-box (gtk:make-box :orientation gtk:+orientation-horizontal+
					      :spacing 4))
	      (exit-btn (gtk:make-button :label "Exit"))
	      (layer-1-btn (gtk:make-button :label "L1"))
	      (layer-2-btn (gtk:make-button :label "L2"))
	      (all-layers-btn (gtk:make-button :label "All")))

          (gtk:connect exit-btn "clicked" (lambda (button)
                                            (declare (ignore button))
                                            (gtk:window-destroy window)
                                            (when *quit-on-close*
                                              (uiop:quit))))

	  (gtk:connect layer-1-btn "clicked" (lambda (button)
                                               (declare (ignore button))
					       (setf *current-layer* 1)
					       (setf (gtk:widget-sensitive-p layer-1-btn) nil)
					       (setf (gtk:widget-sensitive-p layer-2-btn) t)
					       (setf (gtk:widget-sensitive-p all-layers-btn) t)))

	  (gtk:connect layer-2-btn "clicked" (lambda (button)
                                               (declare (ignore button))
					       (setf *current-layer* 2)
					       (setf (gtk:widget-sensitive-p layer-1-btn) t)
					       (setf (gtk:widget-sensitive-p layer-2-btn) nil)
					       (setf (gtk:widget-sensitive-p all-layers-btn) t)))

	  (gtk:connect all-layers-btn "clicked" (lambda (button)
						  (declare (ignore button))
						  (setf *current-layer* 0)
						  (setf (gtk:widget-sensitive-p layer-1-btn) t)
						  (setf (gtk:widget-sensitive-p layer-2-btn) t)
						  (setf (gtk:widget-sensitive-p all-layers-btn) nil)))

	  (setf (gtk:widget-sensitive-p layer-1-btn) nil)

	  (gtk:box-append bottom-right-box all-layers-btn)
	  (gtk:box-append bottom-right-box layer-1-btn)
	  (gtk:box-append bottom-right-box layer-2-btn)
	  (gtk:box-append bottom-right-box exit-btn)

          (gtk:box-append right-box bottom-right-box))

	(gtk:box-append box right-box))

      (setf (gtk:window-child window) box))

    (unless (gtk:widget-visible-p window)
      (gtk:window-present window))))
