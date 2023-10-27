(in-package #:chrono-labyrinth)

(defun clamp (lower-bound value upper-bound)
  (max lower-bound (min value upper-bound)))

(defclass point ()
  ((x :accessor x :initarg :x :initform 0.0 :type float)
   (y :accessor y :initarg :y :initform 0.0 :type float)))

(defun make-point (&key (x 0.0) (y 0.0))
  (make-instance 'point :x x :y y))

(defun point/add (lhs rhs)
  (make-point :x (+ (x lhs) (x rhs))
	      :y (+ (y lhs) (y rhs))))

(defun point/subtract (lhs rhs)
  (make-point :x (- (x lhs) (x rhs))
	      :y (- (y lhs) (y rhs))))

(defmethod print-object ((p point) stream)
  (format stream "pt(~d, ~d)" (x p) (y p)))

(defclass size ()
  ((width :accessor width :initarg :width :initform 0.0 :type float)
   (height :accessor height :initarg :height :initform 0.0 :type float)))

(defun make-size (&key (width 0.0) (height 0.0))
  (make-instance 'size :width width :height height))

(defmethod print-object ((s size) stream)
  (format stream "sz(~d, ~d)" (width s) (height s)))

(defclass rectangle ()
  ((x :accessor x :initarg :x :initform 0.0 :type float)
   (y :accessor y :initarg :y :initform 0.0 :type float)
   (width :accessor width :initarg :width :initform 0.0 :type float)
   (height :accessor height :initarg :height :initform 0.0 :type float)))

(defun make-rectangle (&key (x 0.0) (y 0.0) (width 0.0) (height 0.0))
  (make-instance 'rectangle :x x :y y :width width :height height))

(defun rectangle/intersects? (rect-a rect-b)
  (let* ((rect-a-x1 (x rect-a))
	 (rect-a-x2 (+ rect-a-x1 (width rect-a)))
	 (rect-a-y1 (y rect-a))
	 (rect-a-y2 (+ rect-a-y1 (height rect-a)))

	 (rect-b-x1 (x rect-b))
	 (rect-b-x2 (+ rect-b-x1 (width rect-b)))
	 (rect-b-y1 (y rect-b))
	 (rect-b-y2 (+ rect-b-y1 (height rect-b))))
    (and (< rect-a-x1 rect-b-x2)
	 (> rect-a-x2 rect-b-x1)
	 (< rect-a-y1 rect-b-y2)
	 (> rect-a-y2 rect-b-y1))))

(defun rectangle/right (rect)
  (with-slots (x width) rect
    (+ x width)))

(defun rectangle/bottom (rect)
  (with-slots (y height) rect
    (+ y height)))

(defun rectangle/left (rect)
  (x rect))

(defun rectangle/top (rect)
  (y rect))

(defmethod print-object ((r rectangle) stream)
  (format stream "rt(~d, ~d, ~d, ~d)" (x r) (y r) (width r) (height r)))

(defparameter *camera-position* (make-point))
(defparameter *camera-view-port-size* (make-size))
(defparameter *camera-world-rectangle* (make-rectangle))

(defun camera-position ()
  *camera-position*)

(defun camera-world-rectangle ()
  *camera-world-rectangle*)

(defun (setf camera-world-rectangle) (value)
  (setf *camera-world-rectangle* value))

(defun camera-view-port-width ()
  (width *camera-view-port-size*))

(defun (setf camera-view-port-width) (value)
  (setf (width *camera-view-port-size*) value))

(defun camera-view-port-height ()
  (height *camera-view-port-size*))

(defun (setf camera-view-port-height) (value)
  (setf (height *camera-view-port-size*) value))

(defun camera-view-port ()
  (make-rectangle :x (x (camera-position))
		  :y (y (camera-position))
		  :width (camera-view-port-width)
		  :height (camera-view-port-height)))

(defun (setf camera-position) (value)
  (setf *camera-position*
	(make-point :x (clamp (x (camera-world-rectangle))
			      (x value)
			      (- (width (camera-world-rectangle))
				 (camera-view-port-width)))
		    :y (clamp (y (camera-world-rectangle))
			      (y value)
			      (- (height (camera-world-rectangle))
				 (camera-view-port-height))))))

(defun camera-move (offset)
  (setf (camera-position) (point/add (camera-position) offset)))

(defun camera-object-is-visible? (bounds)
  (rectangle/intersects? (camera-view-port) bounds))

(defgeneric camera-world-to-screen (obj))

(defmethod camera-world-to-screen ((world-location point))
  (point/subtract world-location *camera-position*))

(defmethod camera-world-to-screen ((world-rectangle rectangle))
  (make-rectangle :x (- (rectangle/left world-rectangle) (x *camera-position*))
		  :y (- (rectangle/top world-rectangle) (y *camera-position*))
		  :width (width world-rectangle)
		  :height (height world-rectangle)))

(defgeneric camera-screen-to-world (obj))

(defmethod camera-screen-to-world ((screen-location point))
  (point/add screen-location *camera-position*))

(defmethod camera-screen-to-world ((screen-rectangle rectangle))
  (make-rectangle :x (+ (rectangle/left screen-rectangle) (x *camera-position*))
		  :y (+ (rectangle/top screen-rectangle) (y *camera-position*))
		  :width (width screen-rectangle)
		  :height (height screen-rectangle)))
