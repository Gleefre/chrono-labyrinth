(in-package #:chrono-labyrinth)

(defclass point ()
  ((x :accessor x :initarg :x :initform 0.0 :type float)
   (y :accessor y :initarg :y :initform 0.0 :type float)))

(defclass size ()
  ((width :accessor width :initarg :width :initform 0.0 :type float)
   (height :accessor height :initarg :height :initform 0.0 :type float)))

(defclass rectangle ()
  ((x :accessor x :initarg :x :initform 0.0 :type float)
   (y :accessor y :initarg :y :initform 0.0 :type float)
   (width :accessor width :initarg :width :initform 0.0 :type float)
   (height :accessor height :initarg :height :initform 0.0 :type float)))

(defparameter *camera-position* (make-point))
(defparameter *camera-view-port-size* (make-size))
(defparameter *camera-world-rectangle* (make-rectangle))

(defun camera-position ()
  *camera-position*)

(defun (setf camera-position) (value)
  (setf *camera-position*
	(point :x (->float (clamp (vector-2-x value)
				  (rectangle-x *camera-world-rectangle*)
				  (- (rectangle-width *camera-world-rectangle*)
				     (camera-view-port-width))))
	       :y (->float (clamp (vector-2-y value)
				  (rectangle-y *camera-world-rectangle*)
				  (- (rectangle-height *camera-world-rectangle*)
				     (camera-view-port-height)))))))

(defun camera-world-rectangle ()
  *camera-world-rectangle*)

(defun (setf camera-world-rectangle) (value)
  (setf *camera-world-rectangle* value))

(defun camera-view-port-width ()
  (->int (vector-2-x *camera-view-port-size*)))

(defun (setf camera-view-port-width) (value)
  (setf (vector-2-x *camera-view-port-size*) (->float value)))

(defun camera-view-port-height ()
  (->int (vector-2-y *camera-view-port-size*)))

(defun (setf camera-view-port-height) (value)
  (setf (vector-2-y *camera-view-port-size*) (->float value)))

(defun camera-view-port ()
  (make-rectangle :x (->int (vector-2-x (camera-position)))
		  :y (->int (vector-2-y (camera-position)))
		  :width (camera-view-port-width)
		  :height (camera-view-port-height)))

(defun camera-move (offset)
  (setf (camera-position) (add (camera-position) offset)))

(defun camera-object-is-visible? (bounds)
  (intersects? (camera-view-port) bounds))

(defgeneric camera-world-to-screen (obj))

(defmethod camera-world-to-screen ((world-location vector-2))
  (subtract world-location *camera-position*))

(defmethod camera-world-to-screen ((world-rectangle rectangle))
  (make-rectangle :x (- (rectangle-left world-rectangle) (->int (vector-2-x *camera-position*)))
		  :y (- (rectangle-top world-rectangle) (->int (vector-2-y *camera-position*)))
		  :width (rectangle-width world-rectangle)
		  :height (rectangle-height world-rectangle)))

(defgeneric camera-screen-to-world (obj))

(defmethod camera-screen-to-world ((screen-location vector-2))
  (add screen-location *camera-position*))

(defmethod camera-screen-to-world ((screen-rectangle rectangle))
  (make-rectangle :x (+ (rectangle-left screen-rectangle) (->int (vector-2-x *camera-position*)))
		  :y (+ (rectangle-top screen-rectangle) (->int (vector-2-y *camera-position*)))
		  :width (rectangle-width screen-rectangle)
		  :height (rectangle-height screen-rectangle)))
