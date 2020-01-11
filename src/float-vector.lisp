(in-package :cl-aubio)

(defclass float-vector ()
  ((size :initarg :size :type 'integer)
   (internal-aubio-object :writer internal-aubio-object)))

(defun make-float-vector (length)
  (make-instance 'float-vector :size length))

(defmethod get-data ((a-float-vector float-vector))
  (aubio/bindings::|fvec_get_data| (slot-value a-float-vector 'internal-aubio-object)))

(defmethod initialize-instance :after ((vector float-vector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-aubio-object (aubio/bindings::|new_fvec| (slot-value vector 'size)) vector))

(defmethod clean ((vector float-vector))
  (aubio/bindings::|del_fvec| (slot-value vector 'internal-aubio-object)))

(defmethod aubio-to-lisp ((vector float-vector))
  (cffi:foreign-array-to-lisp (get-data vector) (list :array 'aubio/bindings::|smpl_t| (slot-value vector 'size))))
