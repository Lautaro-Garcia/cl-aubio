(in-package :cl-aubio)

(deftype onset-detection-function ()
  '(member :default :energy :hfc :complex :phase :wphase :specdiff :kl :mkl :specflux))

(defclass onset-detector (aubio-object)
  ((detection-method :initarg :detection-method :type onset-detection-function :initform :default)
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (internal-method :writer internal-method :type cffi:foreign-pointer)))

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

(defmethod silence-threshold ((an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_get_silence| (internal-aubio-object an-onset-detector)))

(defmethod (setf silence-threshold) (a-silence-threshold (an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_set_silence| (internal-aubio-object an-onset-detector)
                                             a-silence-threshold))

(defmethod peak-picking-threshold ((an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_get_threshold| (internal-aubio-object an-onset-detector)))

(defmethod (setf peak-picking-threshold) (a-peak-picking-threshold (an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_set_threshold| (internal-aubio-object an-onset-detector)
                                               a-peak-picking-threshold))

(defun adaptive-whitening-enabled? (an-onset-detector)
  (= (aubio/bindings::|aubio_onset_get_awhitening| (internal-aubio-object an-onset-detector))
     1))

(defun (setf adaptive-whitening-enabled?) (should-use-adaptive-whitening an-onset-detector)
  (aubio/bindings::|aubio_onset_set_awhitening| (internal-aubio-object an-onset-detector)
                                                (if should-use-adaptive-whitening 1 0)))

(defun compression-factor (an-onset-detector)
  (aubio/bindings::|aubio_onset_get_compression| (internal-aubio-object an-onset-detector)))

(defun (setf compression-factor) (compression-factor an-onset-detector)
  (aubio/bindings::|aubio_onset_set_compression| (internal-aubio-object an-onset-detector)
                                                 compression-factor))

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
