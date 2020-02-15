(in-package :cl-aubio)

(defclass float-matrix ()
  ((length :initarg :length :type integer)
   (height :initarg :height :type integer)
   (internal-aubio-object :writer internal-aubio-object :type cffi:foreign-pointer)))

(defun make-float-matrix (length height)
  (make-instance 'float-matrix :length length :height height))

(defmethod initialize-instance :after ((matrix float-matrix) &rest args)
  (declare (ignore args))
  (internal-aubio-object (aubio/bindings::|new_fmat| (slot-value matrix 'height)
                                  (slot-value matrix 'length))
                 matrix))

(defmethod get-data ((a-float-matrix float-matrix))
  (aubio/bindings::|fmat_get_data| (slot-value a-float-matrix 'internal-aubio-object)))

(defmethod clean ((matrix float-matrix))
  (aubio/bindings::|del_fmat| (slot-value matrix 'internal-aubio-object)))

(defun get-channel-data (a-float-matrix a-channel-number)
  (aubio/bindings::|fmat_get_channel_data| (slot-value a-float-matrix 'internal-aubio-object) a-channel-number))

(defmethod aubio-to-lisp ((matrix float-matrix))
  (with-slots (length height) matrix
    (make-array (list height length)
                :initial-contents (loop :for channel :below height
                                        :collecting (cffi:foreign-array-to-lisp (get-channel-data matrix channel) `(:array aubio/bindings::|smpl_t| ,length))))))
