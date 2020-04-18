(in-package :cl-aubio)

(deftype onset-detection-function ()
  '(member :default :energy :hfc :complex :phase :wphase :specdiff :kl :mkl :specflux))

(defclass onset-detector (aubio-object)
  ((detection-method :initarg :detection-method :type onset-detection-function :initform :default)
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (silence-threshold :accessor silence-threshold :aubio-reader |aubio_onset_get_silence| :aubio-writer |aubio_onset_set_silence|)
   (peak-picking-threshold :accessor peak-picking-threshold :aubio-reader |aubio_onset_get_threshold| :aubio-writer |aubio_onset_set_threshold|)
   (compression-factor :accessor compression-factor :aubio-reader |aubio_onset_get_compression| :aubio-writer |aubio_onset_set_compression|)
   (adaptive-whitening-enabled? :accessor adaptive-whitening-enabled? :aubio-reader |aubio_onset_get_awhitening| :aubio-writer |aubio_onset_set_awhitening|)
   (internal-method :writer internal-method :type cffi:foreign-pointer))
  (:metaclass aubio-class))

(defun make-onset-detector (buffer-size hop-size sample-rate &key (method :default))
  (declare (type onset-detection-function method))
  (make-instance 'onset-detector :detection-method method :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((onset-detector onset-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc (string-downcase (slot-value onset-detector 'detection-method))) onset-detector)
  (with-slots (internal-method buffer-size hop-size sample-rate) onset-detector
    (setf (internal-aubio-object onset-detector)
          (aubio/bindings::|new_aubio_onset| internal-method buffer-size hop-size sample-rate))))

(defmethod clean ((onset-detector onset-detector))
  (with-slots (internal-method unit) onset-detector
    (aubio/bindings::|del_aubio_onset| (internal-aubio-object onset-detector))
    (cffi:foreign-string-free internal-method)))

(defun reset-onset-detector (onset-detector)
  (aubio/bindings::|aubio_onset_reset| (internal-aubio-object onset-detector)))

(defmethod adaptive-whitening-enabled? :around ((an-onset-detector onset-detector))
  (= (call-next-method) 1))

(defmethod (setf adaptive-whitening-enabled?) :around (should-use-adaptive-whitening (an-onset-detector onset-detector))
  (call-next-method (if should-use-adaptive-whitening 1 0) an-onset-detector))

(defmethod minimum-inter-onset-interval ((an-onset-detector onset-detector) &key (unit :samples))
  (declare (type timestamp-unit unit))
  (let ((internal-aubio-object (internal-aubio-object an-onset-detector)))
    (cond
      ((eq unit :seconds) (aubio/bindings::|aubio_onset_get_minioi_s| internal-aubio-object))
      ((eq unit :milliseconds) (aubio/bindings::|aubio_onset_get_minioi_ms| internal-aubio-object))
      (t (aubio/bindings::|aubio_onset_get_minioi| internal-aubio-object)))))

(defmethod (setf minimum-inter-onset-interval) (a-minimum-inter-onset-interval (an-onset-detector onset-detector) &key (unit :samples))
  (declare (type timestamp-unit unit))
  (let ((internal-aubio-object (internal-aubio-object an-onset-detector)))
    (cond
      ((eq unit :seconds) (aubio/bindings::|aubio_onset_set_minioi_s| internal-aubio-object a-minimum-inter-onset-interval))
      ((eq unit :milliseconds) (aubio/bindings::|aubio_onset_set_minioi_ms| internal-aubio-object a-minimum-inter-onset-interval))
      (t (aubio/bindings::|aubio_onset_set_minioi| internal-aubio-object a-minimum-inter-onset-interval)))))

(defun time-of-latest-onset-detected (an-onset-detector &key (unit :samples))
  (declare (type timestamp-unit unit))
  (let ((internal-aubio-object (internal-aubio-object an-onset-detector)))
    (cond
      ((eq unit :seconds) (aubio/bindings::|aubio_onset_get_last_s| internal-aubio-object))
      ((eq unit :milliseconds) (aubio/bindings::|aubio_onset_get_last_ms| internal-aubio-object))
      (t (aubio/bindings::|aubio_onset_get_last| internal-aubio-object)))))

(defun detect-onset (an-onset-detector an-input-source &key (unit :seconds))
  (declare (type timestamp-unit unit))
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_onset_do|
                            (internal-aubio-object an-onset-detector)
                            (internal-aubio-object an-input-source)
                            (internal-aubio-object output-vector))
           (when (> (elt (aubio-to-lisp output-vector) 0) 0)
             (time-of-latest-onset-detected an-onset-detector :unit unit)))
       (clean output-vector))))
