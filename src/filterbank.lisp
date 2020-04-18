(in-package :cl-aubio)

(defclass filterbank (aubio-object)
  ((buffer-size :initarg :buffer-size :reader buffer-size :type integer)
   (number-of-filters :initarg :number-of-filters :reader number-of-filters :type integer)
   (normalization-enabled :accessor normalization-enabled? :aubio-reader |aubio_filterbank_get_norm| :aubio-writer |aubio_filterbank_set_norm|)
   (power :accessor power-parameter :aubio-reader |aubio_filterbank_get_power| :aubio-writer |aubio_filterbank_set_power| :type float))
  (:metaclass aubio-class))

(defun make-filterbank (number-of-filters buffer-size)
  (make-instance 'filterbank :number-of-filters number-of-filters :buffer-size buffer-size))

(defmethod initialize-instance :after ((a-filterbank filterbank) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (internal-aubio-object a-filterbank) (aubio/bindings::|new_aubio_filterbank| (number-of-filters a-filterbank)
                                                                                     (buffer-size a-filterbank))))

(defmethod clean ((a-filterbank filterbank))
  (aubio/bindings::|del_aubio_filterbank| (internal-aubio-object a-filterbank)))

(defmethod normalization-enabled? :around (a-filterbank)
  (= (call-next-method) 1.0))

(defmethod (setf normalization-enabled?) :around (status a-filterbank)
  (call-next-method (if status 1.0 0) a-filterbank))

(defun filter-coefficients (a-filterbank)
  "Returns a copy of the filter-coefficients of A-FILTERBANK."
  (let* ((coefficients (aubio/bindings::|aubio_filterbank_get_coeffs| (internal-aubio-object a-filterbank)))
         (width (cffi:foreign-slot-value coefficients 'aubio/bindings::|fmat_t| 'aubio/bindings::|length|))
         (height (cffi:foreign-slot-value coefficients 'aubio/bindings::|fmat_t| 'aubio/bindings::|height|))
         (float-matrix (make-float-matrix width height)))
    (aubio/bindings::|fmat_copy| coefficients (internal-aubio-object float-matrix))
    float-matrix))

;; The Aubio API always returns 0, so there is no way to actually know if the set was successful
(defun (setf filter-coefficients) (coefficients a-filterbank)
  (aubio/bindings::|aubio_filterbank_set_coeffs| (internal-aubio-object a-filterbank)
                                                 (internal-aubio-object coefficients)))

(defun (setf triangle-bands) (frequencies filterbank samplerate)
  (declare (type filterbank filterbank))
  (declare (type float-vector frequencies))
  (declare (type float samplerate))
  (aubio/bindings::|aubio_filterbank_set_triangle_bands| (internal-aubio-object filterbank)
                                                         (internal-aubio-object frequencies)
                                                         samplerate))
