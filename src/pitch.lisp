(in-package :cl-aubio)

(deftype pitch-detection-unit ()
  '(member "Hz" "midi" "cent" "bin"))

(deftype pitch-detection-method ()
  '(member "default" "schmitt" "fcomb" "mcomb" "yin" "yinfast" "yinfft"))

(defclass pitch-detector ()
  ((detection-method :initarg :detection-method :type pitch-detection-method :initform "default")
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (unit :initform "Hz" :type pitch-detection-unit)
   (internal-method :writer internal-method :type cffi:foreign-pointer)
   (internal-pitch :writer internal-pitch :type cffi:foreign-pointer)))

(defclass pitch ()
  ((pitch :initarg :pitch :reader pitch :type string)
   (confidence :initarg :confidence :reader confidence :type float)
   (unit :initarg :unit :reader unit :type pitch-detection-unit :initform "Hz")))

(defun make-pitch-detector (buffer-size hop-size sample-rate &key (method "default"))
  (make-instance 'pitch-detector :detection-method method :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((pitch-detector pitch-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc (slot-value pitch-detector 'detection-method)) pitch-detector)
  (with-slots (internal-method buffer-size hop-size sample-rate) pitch-detector
    (internal-pitch (aubio/bindings::|new_aubio_pitch| internal-method buffer-size hop-size sample-rate)
                    pitch-detector)))

(defmethod clean ((pitch-detector pitch-detector))
  (with-slots (internal-pitch internal-method unit) pitch-detector
    (aubio/bindings::|del_aubio_pitch| internal-pitch)
    (cffi:foreign-string-free internal-method)
    (when unit (cffi:foreign-string-free unit))))

(defun tolerance (a-pitch-detector)
  (aubio/bindings::|aubio_pitch_get_tolerance| (slot-value a-pitch-detector 'internal-pitch)))

(defun (setf tolerance) (a-tolerance a-pitch-detector)
  (aubio/bindings::|aubio_pitch_set_tolerance| (slot-value a-pitch-detector 'internal-pitch)
                                               a-tolerance))

(defmethod confidence ((a-pitch-detector pitch-detector))
  (aubio/bindings::|aubio_pitch_get_confidence| (slot-value a-pitch-detector 'internal-pitch)))

(defmethod silence-threshold ((a-pitch-detector pitch-detector))
  (aubio/bindings::|aubio_pitch_get_silence| (slot-value a-pitch-detector 'internal-pitch)))

(defmethod (setf silence-threshold) (a-silence-threshold (a-pitch-detector pitch-detector))
  (aubio/bindings::|aubio_pitch_set_silence| (slot-value a-pitch-detector 'internal-pitch)
                                             a-silence-threshold))

(defun pitch-detection-unit (a-pitch-detector)
  (let ((unit (slot-value a-pitch-detector 'unit)))
    (when unit (cffi:foreign-string-to-lisp unit))))

(defun (setf pitch-detection-unit) (a-detection-unit a-pitch-detector)
  (let ((c-detection-unit (cffi:foreign-string-alloc a-detection-unit))
        (previous-detection-unit (slot-value a-pitch-detector 'unit)))
    (aubio/bindings::|aubio_pitch_set_unit|
                     (slot-value a-pitch-detector 'internal-pitch)
                     c-detection-unit)
    (when previous-detection-unit (cffi:foreign-string-free previous-detection-unit))
    (setf (slot-value a-pitch-detector 'unit) c-detection-unit)))

(defun detect-pitch (a-pitch-detector an-input-source)
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_pitch_do|
                            (slot-value a-pitch-detector 'internal-pitch)
                            (slot-value an-input-source 'internal-aubio-object)
                            (slot-value output-vector 'internal-aubio-object))
           (make-instance 'pitch :pitch (elt (aubio-to-lisp output-vector) 0)
                                 :confidence (confidence a-pitch-detector)
                                 :unit (or (pitch-detection-unit a-pitch-detector) "Hz")))
      (clean output-vector))))
