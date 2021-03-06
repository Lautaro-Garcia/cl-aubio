(in-package :cl-aubio)

(defclass source (aubio-object)
  ((uri :initarg :uri :reader uri :type sting)
   (sample-rate :initarg :sample-rate :type 'integer)
   (hop-size :reader hop-size :initarg :hop-size :type 'integer)
   (number-of-channels :reader number-of-channels :aubio-reader |aubio_source_get_channels|)
   (duration :reader duration :aubio-reader |aubio_source_get_duration|)
   (samplerate :reader samplerate :aubio-reader |aubio_source_get_samplerate|))
  (:metaclass aubio-class))

(defun make-source (uri hop-size &optional (sample-rate 0))
  (make-instance 'source :uri uri :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((source source) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (with-slots (uri sample-rate hop-size) source
    (cffi:with-foreign-string (c-uri uri)
      (setf (internal-aubio-object source)
            (aubio/bindings::|new_aubio_source| c-uri sample-rate hop-size)))))

(defun read-source (a-source &key as-lisp-vector (monophonic t))
  (with-slots (internal-aubio-object hop-size) a-source
    (let ((aubio-read-function (if monophonic #'aubio/bindings::|aubio_source_do| #'aubio/bindings::|aubio_source_do_multi|))
          (samples (if monophonic (make-float-vector hop-size) (make-float-matrix hop-size (number-of-channels a-source)))))
      (unwind-protect
        (cffi:with-foreign-object (read-count-pointer '(:pointer aubio/bindings::|uint_t|))
          (funcall aubio-read-function internal-aubio-object (internal-aubio-object samples) read-count-pointer)
          (let* ((read-count (cffi:mem-aref read-count-pointer 'aubio/bindings::|uint_t|))
                 (samples-return-vector (if as-lisp-vector (aubio-to-lisp samples) samples)))
            (values samples-return-vector read-count)))
        (when as-lisp-vector (clean samples))))))

(define-condition seek-error (error)
  ((source :initarg :source :reader seek-source)
   (position :initarg :position :reader seek-position))
  (:report (lambda (condition stream) (format stream "Aubio couldn't seek position ~a for the source ~a"
                                              (seek-position condition)
                                              (uri (seek-source condition))))))

(defun seek (a-source a-position)
  (let ((seek-ok (aubio/bindings::|aubio_source_seek| (internal-aubio-object a-source) a-position)))
    (unless seek-ok (error 'seek-error :source a-source :position a-position))))

(defmethod clean ((source source))
  (aubio/bindings::|del_aubio_source| (internal-aubio-object source)))

(defmacro with-source ((variable uri hop-size &key (sample-rate 0)) &body body)
  `(let ((,variable (make-source ,uri ,hop-size ,sample-rate)))
     (unwind-protect (progn ,@body) (clean ,variable))))

(defmacro do-source ((samples-variable uri hop-size &key (sample-rate 0) (monophonic t) (amount-read-variable (gensym)) (source-variable (gensym)) as-lisp-vector) &body body)
  `(with-source (,source-variable ,uri ,hop-size :sample-rate ,sample-rate)
     (loop :for (,samples-variable , amount-read-variable) := (multiple-value-list (read-source ,source-variable :monophonic ,monophonic
                                                                                                                 :as-lisp-vector ,as-lisp-vector))
           :while (= ,amount-read-variable ,hop-size)
           :do ,@body)))
