(in-package :cl-aubio)

(defclass timestamp ()
  ((value :initarg :value :reader value :type 'float)
   (unit :initarg :unit :reader unit :type 'symbol)))

(defun make-timestamp (value unit)
  (make-instance 'timestamp :value value :unit unit))
