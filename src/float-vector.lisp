(in-package :cl-aubio)

(defclass float-vector (aubio-object)
  ((size :initarg :size :reader size :type integer)))

(defun make-float-vector (length)
  (make-instance 'float-vector :size length))

(defmethod get-data ((a-float-vector float-vector))
  (aubio/bindings::|fvec_get_data| (internal-aubio-object a-float-vector)))

(defmethod initialize-instance :after ((vector float-vector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (internal-aubio-object vector) (aubio/bindings::|new_fvec| (slot-value vector 'size))))

(defmethod clean ((vector float-vector))
  (aubio/bindings::|del_fvec| (internal-aubio-object vector)))

(defmethod aubio-to-lisp ((vector float-vector))
  (cffi:foreign-array-to-lisp (get-data vector) (list :array 'aubio/bindings::|smpl_t| (slot-value vector 'size))))
