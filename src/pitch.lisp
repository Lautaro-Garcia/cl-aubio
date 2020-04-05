(in-package :cl-aubio)

(deftype pitch-detection-unit ()
  '(member :hz :midi :cent :bin))

(deftype pitch-detection-method ()
  '(member :default :schmitt :fcomb :mcomb :yin :yinfast :yinfft))

(defclass pitch-detector (aubio-object)
  ((detection-method :initarg :detection-method :type pitch-detection-method :initform :default)
   (buffer-size :initarg :buffer-size :type integer)
   (hop-size :initarg :hop-size :type integer)
   (sample-rate :initarg :sample-rate :type integer)
   (unit :initform (cffi:foreign-string-alloc "Hz") :type cffi:foreign-pointer)
   (tolerance :accessor tolerance :aubio-reader |aubio_pitch_get_tolerance| :aubio-writer |aubio_pitch_set_tolerance|)
   (confidence :reader confidence :aubio-reader |aubio_pitch_get_silence|)
   (silence-threshold :accessor silence-threshold :aubio-reader |aubio_pitch_get_silence| :aubio-writer |aubio_pitch_set_silence|)
   (internal-method :writer internal-method :type cffi:foreign-pointer))
  (:metaclass aubio-class))

(defclass pitch ()
  ((pitch :initarg :pitch :reader pitch :type string)
   (confidence :initarg :confidence :reader confidence :type float)
   (unit :initarg :unit :reader unit :type pitch-detection-unit :initform :hz)))

(defun make-pitch-detector (buffer-size hop-size sample-rate &key (method :default))
  (declare (type pitch-detection-method method))
  (make-instance 'pitch-detector :detection-method method :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((pitch-detector pitch-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc (string-downcase (slot-value pitch-detector 'detection-method))) pitch-detector)
  (with-slots (internal-method buffer-size hop-size sample-rate) pitch-detector
    (setf (internal-aubio-object pitch-detector)
          (aubio/bindings::|new_aubio_pitch| internal-method buffer-size hop-size sample-rate))))

(defmethod clean ((pitch-detector pitch-detector))
  (with-slots (internal-method unit) pitch-detector
    (aubio/bindings::|del_aubio_pitch| (internal-aubio-object pitch-detector))
    (cffi:foreign-string-free internal-method)
    (when unit (cffi:foreign-string-free unit))))

(defun pitch-detection-unit (a-pitch-detector)
  (let ((unit (slot-value a-pitch-detector 'unit)))
    (when unit (cffi:foreign-string-to-lisp unit))))

(defun (setf pitch-detection-unit) (a-detection-unit a-pitch-detector)
  (declare (type pitch-detection-unit a-detection-unit))
  (let ((c-detection-unit (cffi:foreign-string-alloc (string-downcase a-detection-unit)))
        (previous-detection-unit (slot-value a-pitch-detector 'unit)))
    (aubio/bindings::|aubio_pitch_set_unit|
                     (internal-aubio-object a-pitch-detector)
                     c-detection-unit)
    (when previous-detection-unit (cffi:foreign-string-free previous-detection-unit))
    (setf (slot-value a-pitch-detector 'unit) c-detection-unit)))

(defun detect-pitch (a-pitch-detector an-input-source)
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_pitch_do|
                            (internal-aubio-object a-pitch-detector)
                            (internal-aubio-object an-input-source)
                            (internal-aubio-object output-vector))
           (make-instance 'pitch :pitch (elt (aubio-to-lisp output-vector) 0)
                                 :confidence (confidence a-pitch-detector)
                                 :unit (or (pitch-detection-unit a-pitch-detector) "Hz")))
      (clean output-vector))))
