(in-package :cl-aubio)

(defclass float-vector (aubio-object)
  ((size :initarg :size :reader size :type integer)
   (data :reader get-data :aubio-reader |fvec_get_data|))
  (:metaclass aubio-class))

(defun make-float-vector (length)
  (make-instance 'float-vector :size length))

(defun make-float-vector-from-lisp-vector (a-lisp-vector)
  (let ((float-vector (make-float-vector (length a-lisp-vector))))
    (loop :for i :below (length a-lisp-vector)
          :do (setf (get-vector-sample float-vector i) (aref a-lisp-vector i)))
    float-vector))

(defmethod initialize-instance :after ((vector float-vector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (internal-aubio-object vector) (aubio/bindings::|new_fvec| (slot-value vector 'size))))

(defmethod clean ((vector float-vector))
  (aubio/bindings::|del_fvec| (internal-aubio-object vector)))

(defmethod aubio-to-lisp ((vector float-vector))
  (cffi:foreign-array-to-lisp (get-data vector) (list :array 'aubio/bindings::|smpl_t| (slot-value vector 'size))))

(defun get-vector-sample (float-vector index)
  (aubio/bindings::|fvec_get_sample| (internal-aubio-object float-vector) index))

(defun (setf get-vector-sample) (data float-vector index)
  (aubio/bindings::|fvec_set_sample| (internal-aubio-object float-vector) data index))

(defmethod set-all-samples ((float-vector float-vector) sample)
  (aubio/bindings::|fvec_set_all| (internal-aubio-object float-vector) sample))

(defmethod set-all-samples-to-zero ((float-vector float-vector))
  (aubio/bindings::|fvec_zeros| (internal-aubio-object float-vector)))

(defmethod set-all-samples-to-one ((float-vector float-vector))
  (aubio/bindings::|fvec_ones| (internal-aubio-object float-vector)))

(defun reverse-vector (float-vector)
  (aubio/bindings::|fvec_rev| (internal-aubio-object float-vector))
  float-vector)

(defmethod weight-vector ((float-vector float-vector) (weight-vector vector))
  (declare (type (array float *) weight-vector))
  (let ((aubio-weight-vector (make-float-vector-from-lisp-vector weight-vector)))
    (aubio/bindings::|fvec_weight| (internal-aubio-object float-vector)
                                   (internal-aubio-object aubio-weight-vector))
    (clean aubio-weight-vector))
  float-vector)

(defmethod weight-vector ((float-vector float-vector) (weight float))
  (weight-vector float-vector (make-array (size float-vector) :initial-element weight)))

(defmethod copy ((float-vector float-vector) &key (to (make-float-vector (size float-vector))))
  (aubio/bindings::|fvec_copy| (internal-aubio-object float-vector)
                               (internal-aubio-object to))
  to)

(defun weighted-copy (float-vector weight-vector)
  (let ((copy (make-float-vector (size float-vector)))
        (aubio-weight-vector (make-float-vector-from-lisp-vector weight-vector)))
    (aubio/bindings::|fvec_weighted_copy| (internal-aubio-object float-vector)
                                          (internal-aubio-object aubio-weight-vector)
                                          (internal-aubio-object copy))
   copy))
