(in-package :cl-aubio)

(defclass note-detector ()
   ((buffer-size :initarg :buffer-size :type integer)
    (hop-size :initarg :hop-size :type integer)
    (sample-rate :initarg :sample-rate :type integer)
    (internal-method :writer internal-method :type cffi:foreign-pointer)
    (internal-note :writer internal-note :type cffi:foreign-pointer)))

(defclass note ()
  ((midi-note :initarg :midi-note :reader midi-note :type integer)
   (velocity :initarg :velocity :reader velocity :type float)))

(defclass note-off (note) ())
(defclass note-on (note) ())

(defun make-note-detector (buffer-size hop-size sample-rate)
  (make-instance 'note-detector :buffer-size buffer-size :hop-size hop-size :sample-rate sample-rate))

(defmethod initialize-instance :after ((note-detector note-detector) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (internal-method (cffi:foreign-string-alloc "default") note-detector)
  (with-slots (buffer-size hop-size sample-rate internal-method) note-detector
    (internal-note (aubio/bindings::|new_aubio_notes| internal-method buffer-size hop-size sample-rate)
                   note-detector)))

(defmethod clean ((note-detector note-detector))
  (with-slots (internal-note internal-method) note-detector
    (aubio/bindings::|del_aubio_notes| internal-note)
    (cffi:foreign-string-free internal-method)))

(defmethod silence-threshold ((a-note-detector note-detector))
  (aubio/bindings::|aubio_notes_get_silence| (slot-value a-note-detector 'internal-note)))

(defmethod (setf silence-threshold) (a-silence-threshold (a-note-detector note-detector))
  (aubio/bindings::|aubio_notes_set_silence| (slot-value a-note-detector 'internal-note)
                                             a-silence-threshold))

;; Hope the C API would let me ask for different units, as it does with the onset object
(defun minimum-inter-onset-interval-ms (a-note-detector)
  (let ((internal-note (slot-value a-note-detector 'internal-note)))
    (aubio/bindings::|aubio_notes_get_minioi_ms| internal-note)))

(defun (setf minimum-inter-onset-interval-ms) (a-minimum-inter-onset-interval a-note-detector)
  (let ((internal-note (slot-value a-note-detector 'internal-note)))
    (aubio/bindings::|aubio_notes_set_minioi_ms| internal-note a-minimum-inter-onset-interval)))

(defun release-drop (a-note-detector)
  "Get notes object release drop level in dB "
  (let ((internal-note (slot-value a-note-detector 'internal-note)))
    (aubio/bindings::|aubio_notes_get_release_drop| internal-note)))

(defun (setf release-drop) (a-release-drop a-note-detector)
  "This function sets the release_drop_level parameter, in dB. When a new note is found, the current level in dB is measured. If the measured level drops under that initial level - release_drop_level, then a note-off will be emitted."
  (let ((internal-note (slot-value a-note-detector 'internal-note)))
    (aubio/bindings::|aubio_notes_set_release_drop| internal-note a-release-drop)))

(defun detect-note (a-note-detector an-input-source)
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_notes_do|
                            (slot-value a-note-detector 'internal-note)
                            (slot-value an-input-source 'internal-aubio-object)
                            (slot-value output-vector 'internal-aubio-object))
           (let* ((note-as-vector (aubio-to-lisp output-vector))
                  (note-on (elt note-as-vector 0))
                  (velocity (elt note-as-vector 1))
                  (note-off (elt note-as-vector 2)))
             (cond
               ((> note-on 0) (make-instance 'note-on :midi-note note-on :velocity velocity))
               ((> note-off 0) (make-instance 'note-off :midi-note note-off :velocity velocity))
               (t nil))))
      (clean output-vector))))
