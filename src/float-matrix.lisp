(in-package :cl-aubio)

(defclass float-matrix (aubio-object)
  ((length :initarg :length :type integer)
   (height :initarg :height :type integer)
   (data :reader get-data :aubio-reader |fmat_get_data|))
  (:metaclass aubio-class))

(defun make-float-matrix (length height)
  (make-instance 'float-matrix :length length :height height))

(defmethod initialize-instance :after ((matrix float-matrix) &rest args)
  (declare (ignore args))
  (with-slots (height length) matrix
    (setf (internal-aubio-object matrix) (aubio/bindings::|new_fmat| height length))))

(defmethod clean ((matrix float-matrix))
  (aubio/bindings::|del_fmat| (internal-aubio-object matrix)))

(defun get-channel-data (a-float-matrix a-channel-number)
  (aubio/bindings::|fmat_get_channel_data| (internal-aubio-object a-float-matrix) a-channel-number))

(defmethod aubio-to-lisp ((matrix float-matrix))
  (with-slots (length height) matrix
    (make-array (list height length)
                :initial-contents (loop :for channel :below height
                                        :collecting (cffi:foreign-array-to-lisp (get-channel-data matrix channel) `(:array aubio/bindings::|smpl_t| ,length))))))
