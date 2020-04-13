(in-package :cl-aubio)

(defclass float-matrix (aubio-object)
  ((width :initarg :width :reader width :type integer)
   (height :initarg :height :reader height :type integer)
   (data :reader get-data :aubio-reader |fmat_get_data|))
  (:metaclass aubio-class))

(defun make-float-matrix (width height)
  (make-instance 'float-matrix :width width :height height))

(defun make-float-matrix-from-array (an-array)
  (declare (type (array float 2) an-array))
  (destructuring-bind (height width) (array-dimensions an-array)
    (let ((float-matrix (make-float-matrix width height)))
      (loop :for i :below width :do
        (loop :for j :below height :do
              (aubio/bindings::|fmat_set_sample| (internal-aubio-object float-matrix)
                                                 (aref an-array j i)
                                                 j
                                                 i)))
      float-matrix)))

(defmethod initialize-instance :after ((matrix float-matrix) &rest args)
  (declare (ignore args))
  (with-slots (height width) matrix
    (setf (internal-aubio-object matrix) (aubio/bindings::|new_fmat| height width))))

(defmethod clean ((matrix float-matrix))
  (aubio/bindings::|del_fmat| (internal-aubio-object matrix)))

(defun get-channel-data (a-float-matrix a-channel-number &key as-lisp-vector)
  (declare (type boolean as-lisp-vector)
           (type integer a-channel-number))
  (let ((aubio-float-vector (aubio/bindings::|fmat_get_channel_data| (internal-aubio-object a-float-matrix) a-channel-number)))
    (if as-lisp-vector
        (cffi:foreign-array-to-lisp aubio-float-vector `(:array aubio/bindings::|smpl_t| ,(width a-float-matrix)))
        aubio-float-vector)))

(defun get-sample (float-matrix channel position)
  (aubio/bindings::|fmat_get_sample| (internal-aubio-object float-matrix) channel position))

(defun (setf get-sample) (sample float-matrix channel position)
  (aubio/bindings::|fmat_set_sample| (internal-aubio-object float-matrix) sample channel position))

(defmethod set-all-samples ((float-matrix float-matrix) sample)
  (aubio/bindings::|fmat_set| (internal-aubio-object float-matrix) sample))

(defmethod set-all-samples-to-zero ((float-matrix float-matrix))
  (aubio/bindings::|fmat_zeros| (internal-aubio-object float-matrix)))

(defmethod set-all-samples-to-one ((float-matrix float-matrix))
  (aubio/bindings::|fmat_ones| (internal-aubio-object float-matrix)))

(defun reverse-matrix (float-matrix)
  (aubio/bindings::|fmat_rev| (internal-aubio-object float-matrix))
  float-matrix)

;; The Aubio API expexts a matrix of weight, but only use the first dimension
(defmethod weight-matrix ((float-matrix float-matrix) (weight-vector vector))
  (declare (type (array float *) weight-vector))
  (let ((weight-matrix (make-float-matrix-from-array (make-array (list (height float-matrix) (width float-matrix)) :initial-contents (loop :for x :below (height float-matrix) :collecting  weight-vector)))))
    (aubio/bindings::|fmat_weight| (internal-aubio-object float-matrix) (internal-aubio-object weight-matrix))
    (clean weight-matrix))
  float-matrix)

(defmethod weight-matrix ((float-matrix float-matrix) (weight float))
  (weight-matrix float-matrix (make-array (width float-matrix) :initial-element weight)))

(defun weight-matrix-row (float-matrix row-index factor)
  (loop :for column-index :below (width float-matrix) :do (setf (get-sample float-matrix row-index column-index)
                                                                (* factor (get-sample float-matrix row-index column-index))))
  float-matrix)

(defmethod copy ((float-matrix float-matrix) &key (to (make-float-matrix (width float-matrix) (height float-matrix))))
  (aubio/bindings::|fmat_copy| (internal-aubio-object float-matrix) (internal-aubio-object to))
  to)

(defun multiply-float-matrix-with-vector (float-matrix float-vector)
  (let ((result (make-float-vector (height float-matrix))))
    (aubio/bindings::|fmat_vecmul| (internal-aubio-object float-matrix)
                                   (internal-aubio-object float-vector)
                                   (internal-aubio-object result))
    result))


(defmethod aubio-to-lisp ((matrix float-matrix))
  (make-array (list (height matrix) (width matrix))
              :initial-contents (loop :for channel :below (height matrix)
                                      :collecting (get-channel-data matrix channel :as-lisp-vector t))))
