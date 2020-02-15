(in-package :cl-aubio)

(defclass tempo-detector ()
  ((detection-method :initarg :detection-method :type string)
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (internal-method :writer internal-method :type cffi:foreign-pointer)
   (internal-tempo :writer internal-tempo :type cffi:foreign-pointer)))

(defclass tempo ()
  ((last-beat-position :initarg :last :reader last-beat-position :type float)
   (bpm :initarg :bpm :reader bpm :type integer)
   (confidence :initarg :confidence :reader confidence :type integer)))

(defun make-tempo-detector (method buffer-size hop-size sample-rate)
  (make-instance 'tempo-detector :detection-method method :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((tempo-detector tempo-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc (slot-value tempo-detector 'detection-method)) tempo-detector)
  (with-slots (internal-method buffer-size hop-size sample-rate) tempo-detector
    (internal-tempo (aubio/bindings::|new_aubio_tempo| internal-method buffer-size hop-size sample-rate)
                    tempo-detector)))

(defmethod clean ((tempo-detector tempo-detector))
  (with-slots (internal-tempo internal-method unit) tempo-detector
    (aubio/bindings::|del_aubio_tempo| internal-tempo)
    (cffi:foreign-string-free internal-method)))

(defmethod silence-threshold ((a-tempo-detector tempo-detector))
  (aubio/bindings::|aubio_tempo_get_silence| (slot-value a-tempo-detector 'internal-tempo)))

(defmethod (setf silence-threshold) (a-silence-threshold (a-tempo-detector tempo-detector))
  (aubio/bindings::|aubio_tempo_set_silence| (slot-value a-tempo-detector 'internal-tempo)
                                             a-silence-threshold))

(defmethod peak-picking-threshold ((a-tempo-detector tempo-detector))
  (aubio/bindings::|aubio_tempo_get_threshold| (slot-value a-tempo-detector 'internal-tempo)))

(defmethod (setf peak-picking-threshold) (a-peak-picking-threshold (a-tempo-detector tempo-detector))
  (aubio/bindings::|aubio_tempo_set_threshold| (slot-value a-tempo-detector 'internal-tempo)
                                               a-peak-picking-threshold))

(defmethod bpm ((a-tempo-detector tempo-detector))
  (aubio/bindings::|aubio_tempo_get_bpm| (slot-value a-tempo-detector 'internal-tempo)))

(defmethod confidence ((a-tempo-detector tempo-detector))
  (aubio/bindings::|aubio_tempo_get_confidence| (slot-value a-tempo-detector 'internal-tempo)))

(defun time-of-latest-beat-detected (a-tempo-detector &key (unit 'samples))
  (let ((internal-tempo (slot-value a-tempo-detector 'internal-tempo)))
    (cond
      ((eq unit 'seconds) (aubio/bindings::|aubio_tempo_get_last_s| internal-tempo))
      ((eq unit 'milliseconds) (aubio/bindings::|aubio_tempo_get_last_ms| internal-tempo))
      (t (aubio/bindings::|aubio_tempo_get_last| internal-tempo)))))

(defun detect-tempo (a-tempo-detector an-input-source &key (unit 'seconds))
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_tempo_do|
                            (slot-value a-tempo-detector 'internal-tempo)
                            (slot-value an-input-source 'internal-aubio-object)
                            (slot-value output-vector 'internal-aubio-object))
           (when (> (elt (aubio-to-lisp output-vector) 0) 0)
             (make-instance 'tempo :last (make-timestamp (time-of-latest-beat-detected a-tempo-detector :unit unit) unit)
                                   :bpm (bpm a-tempo-detector)
                                   :confidence (confidence a-tempo-detector))))
       (clean output-vector))))
