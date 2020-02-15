(in-package :cl-aubio)

(deftype onset-detection-function ()
  '(member "default" "energy" "hfc" "complex" "phase" "wphase" "specdiff" "kl" "mkl" "specflux"))

(defclass onset-detector ()
  ((detection-method :initarg :detection-method :type onset-detection-function)
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (internal-method :writer internal-method :type cffi:foreign-pointer)
   (internal-onset :writer internal-onset :type cffi:foreign-pointer)))

(defun make-onset-detector (method buffer-size hop-size sample-rate)
  (make-instance 'onset-detector :detection-method method :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((onset-detector onset-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc (slot-value onset-detector 'detection-method)) onset-detector)
  (with-slots (internal-method buffer-size hop-size sample-rate) onset-detector
    (internal-onset (aubio/bindings::|new_aubio_onset| internal-method buffer-size hop-size sample-rate)
                    onset-detector)))

(defmethod clean ((onset-detector onset-detector))
  (with-slots (internal-onset internal-method unit) onset-detector
    (aubio/bindings::|del_aubio_onset| internal-onset)
    (cffi:foreign-string-free internal-method)))

(defun reset-onset-detector (onset-detector)
  (aubio/bindings::|aubio_onset_reset| (slot-value onset-detector 'internal-onset)))

(defmethod silence-threshold ((an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_get_silence| (slot-value an-onset-detector 'internal-onset)))

(defmethod (setf silence-threshold) (a-silence-threshold (an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_set_silence| (slot-value an-onset-detector 'internal-onset)
                                             a-silence-threshold))

(defmethod peak-picking-threshold ((an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_get_threshold| (slot-value an-onset-detector 'internal-onset)))

(defmethod (setf peak-picking-threshold) (a-peak-picking-threshold (an-onset-detector onset-detector))
  (aubio/bindings::|aubio_onset_set_threshold| (slot-value an-onset-detector 'internal-onset
                                                a-peak-picking-threshold)))

(defun adaptive-whitening-enabled? (an-onset-detector)
  (= (aubio/bindings::|aubio_onset_get_awhitening| (slot-value an-onset-detector 'internal-onset))
     1))

(defun (setf adaptive-whitening-enabled?) (should-use-adaptive-whitening an-onset-detector)
  (aubio/bindings::|aubio_onset_set_awhitening| (slot-value an-onset-detector 'internal-onset)
                                                (if should-use-adaptive-whitening 1 0)))

(defun compression-factor (an-onset-detector)
  (aubio/bindings::|aubio_onset_get_compression| (slot-value an-onset-detector 'internal-onset)))

(defun (setf compression-factor) (compression-factor an-onset-detector)
  (aubio/bindings::|aubio_onset_set_compression| (slot-value an-onset-detector 'internal-onset)
                                                 compression-factor))

(defun minimum-inter-onset-interval (an-onset-detector &key (unit 'samples))
  (declare (type timestamp-unit unit))
  (let ((internal-onset (slot-value an-onset-detector 'internal-onset)))
    (cond
      ((eq unit 'seconds) (aubio/bindings::|aubio_onset_get_minioi_s| internal-onset))
      ((eq unit 'milliseconds) (aubio/bindings::|aubio_onset_get_minioi_ms| internal-onset))
      (t (aubio/bindings::|aubio_onset_get_minioi| internal-onset)))))

(defun (setf minimum-inter-onset-interval) (a-minimum-inter-onset-interval an-onset-detector &key (unit 'samples))
  (declare (type timestamp-unit unit))
  (let ((internal-onset (slot-value an-onset-detector 'internal-onset)))
    (cond
      ((eq unit 'seconds) (aubio/bindings::|aubio_onset_set_minioi_s| internal-onset a-minimum-inter-onset-interval))
      ((eq unit 'milliseconds) (aubio/bindings::|aubio_onset_set_minioi_ms| internal-onset a-minimum-inter-onset-interval))
      (t (aubio/bindings::|aubio_onset_set_minioi| internal-onset a-minimum-inter-onset-interval)))))

(defun time-of-latest-onset-detected (an-onset-detector &key (unit 'samples))
  (declare (type timestamp-unit unit))
  (let ((internal-onset (slot-value an-onset-detector 'internal-onset)))
    (cond
      ((eq unit 'seconds) (aubio/bindings::|aubio_onset_get_last_s| internal-onset))
      ((eq unit 'milliseconds) (aubio/bindings::|aubio_onset_get_last_ms| internal-onset))
      (t (aubio/bindings::|aubio_onset_get_last| internal-onset)))))

(defun detect-onset (an-onset-detector an-input-source &key (unit 'seconds))
  (declare (type timestamp-unit unit))
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_onset_do|
                            (slot-value an-onset-detector 'internal-onset)
                            (slot-value an-input-source 'internal-aubio-object)
                            (slot-value output-vector 'internal-aubio-object))
           (when (> (elt (aubio-to-lisp output-vector) 0) 0)
             (time-of-latest-onset-detected an-onset-detector :unit unit)))
       (clean output-vector))))
