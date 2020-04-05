(in-package :cl-aubio)

(defclass note-detector (aubio-object)
   ((buffer-size :initarg :buffer-size :type integer)
    (hop-size :initarg :hop-size :type integer)
    (sample-rate :initarg :sample-rate :type integer)
    (silence-threshold :accessor silence-threshold :aubio-reader |aubio_notes_get_silence| :aubio-writer |aubio_notes_set_silence|)
    ;; Hope the C API would let me ask for different units, as it does with the onset object
    (minimum-inter-onset-interval :accessor minimum-inter-onset-interval-ms :aubio-reader |aubio_notes_get_minioi_ms| :aubio-writer |aubio_notes_set_minioi_ms|)
    (release-drop :accessor release-drop :aubio-reader |aubio_notes_get_release_drop| :aubio-writer |aubio_notes_set_release_drop|)
    (internal-method :writer internal-method :type cffi:foreign-pointer))
   (:metaclass aubio-class))

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
    (setf (internal-aubio-object note-detector)
          (aubio/bindings::|new_aubio_notes| internal-method buffer-size hop-size sample-rate))))

(defmethod clean ((note-detector note-detector))
  (aubio/bindings::|del_aubio_notes| (internal-aubio-object note-detector))
  (cffi:foreign-string-free (slot-value note-detector 'internal-method)))

(defun detect-note (a-note-detector an-input-source)
  (let ((output-vector (make-float-vector (size an-input-source))))
    (unwind-protect
         (progn
           (aubio/bindings::|aubio_notes_do|
                            (internal-aubio-object a-note-detector)
                            (internal-aubio-object an-input-source)
                            (internal-aubio-object output-vector))
           (let* ((note-as-vector (aubio-to-lisp output-vector))
                  (note-on (elt note-as-vector 0))
                  (velocity (elt note-as-vector 1))
                  (note-off (elt note-as-vector 2)))
             (cond
               ((> note-on 0) (make-instance 'note-on :midi-note note-on :velocity velocity))
               ((> note-off 0) (make-instance 'note-off :midi-note note-off :velocity velocity))
               (t nil))))
      (clean output-vector))))
