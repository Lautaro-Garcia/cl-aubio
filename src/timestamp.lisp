(in-package :cl-aubio)

(deftype timestamp-unit ()
  '(member samples seconds milliseconds))

(defclass timestamp ()
  ((value :initarg :value :reader value :type float)
   (unit :initarg :unit :reader unit :type timestamp-unit)))

(defun make-timestamp (value unit)
  (make-instance 'timestamp :value value :unit unit))
