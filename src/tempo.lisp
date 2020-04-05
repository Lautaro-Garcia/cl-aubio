(in-package :cl-aubio)

(defclass tempo-detector (aubio-object)
  ((detection-method :initarg :detection-method :initform "default" :type string)
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (silence-threshold :accessor silence-threshold :aubio-reader |aubio_tempo_get_silence| :aubio-writer |aubio_tempo_set_silence|)
   (peak-picking-threshold :accessor peak-picking-threshold :aubio-reader |aubio_tempo_get_threshold| :aubio-writer |aubio_tempo_set_threshold|)
   (bpm :reader bpm :aubio-reader |aubio_tempo_get_bpm|)
   (confidence :reader confidence :aubio-reader |aubio_tempo_get_confidence|)
   (internal-method :writer internal-method :type cffi:foreign-pointer))
  (:metaclass aubio-class))

(defclass tempo ()
  ((last-beat-position :initarg :last :reader last-beat-position :type timestamp)
   (bpm :initarg :bpm :reader bpm :type integer)
   (confidence :initarg :confidence :reader confidence :type integer)))

(defun make-tempo-detector (buffer-size hop-size sample-rate)
  (make-instance 'tempo-detector :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((tempo-detector tempo-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc (slot-value tempo-detector 'detection-method)) tempo-detector)
  (with-slots (internal-method buffer-size hop-size sample-rate) tempo-detector
    (setf (internal-aubio-object tempo-detector)
          (aubio/bindings::|new_aubio_tempo| internal-method buffer-size hop-size sample-rate))))

(defmethod clean ((tempo-detector tempo-detector))
  (with-slots (internal-method unit) tempo-detector
    (aubio/bindings::|del_aubio_tempo| (internal-aubio-object tempo-detector))
    (cffi:foreign-string-free internal-method)))

(defun time-of-latest-beat-detected (a-tempo-detector &key (unit 'samples))
  (let ((internal-aubio-object (internal-aubio-object a-tempo-detector)))
    (cond
      ((eq unit 'seconds) (aubio/bindings::|aubio_tempo_get_last_s| internal-aubio-object))
      ((eq unit 'milliseconds) (aubio/bindings::|aubio_tempo_get_last_ms| internal-aubio-object))
      (t (aubio/bindings::|aubio_tempo_get_last| internal-aubio-object)))))

(defun detect-tempo (a-tempo-detector an-input-source &key (unit 'seconds))
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_tempo_do|
                            (internal-aubio-object a-tempo-detector)
                            (internal-aubio-object an-input-source)
                            (internal-aubio-object output-vector))
           (when (> (elt (aubio-to-lisp output-vector) 0) 0)
             (make-instance 'tempo :last (make-timestamp (time-of-latest-beat-detected a-tempo-detector :unit unit) unit)
                                   :bpm (bpm a-tempo-detector)
                                   :confidence (confidence a-tempo-detector))))
       (clean output-vector))))
