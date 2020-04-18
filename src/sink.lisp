(in-package :cl-aubio)

(define-condition sink-samplerate-preset-error (error)
  ((samplerate :initarg :samplerate)))

(define-condition sink-number-of-channels-preset-error (error)
  ((number-of-channels :initarg :number-of-channels)))

(defclass sink (aubio-object)
  ((uri :initarg :uri :reader uri :type sting)
   (internal-samplerate :initarg :samplerate :type integer)
   (samplerate :accessor samplerate :type integer :aubio-reader |aubio_sink_get_samplerate| :aubio-writer |aubio_sink_preset_samplerate|)
   (number-of-channels :accessor number-of-channels :type integer :aubio-reader |aubio_sink_get_channels| :aubio-writer |aubio_sink_preset_channels|))
  (:metaclass aubio-class))

(defun make-sink (output-filename samplerate)
  (make-instance 'sink :uri output-filename :samplerate samplerate))

(defmethod initialize-instance :after ((sink sink) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (with-slots (uri internal-samplerate) sink
    (cffi:with-foreign-string (c-uri uri)
      (setf (internal-aubio-object sink)
            (aubio/bindings::|new_aubio_sink| c-uri internal-samplerate)))))

(defmethod clean ((sink sink))
  (aubio/bindings::|del_aubio_sink| (internal-aubio-object sink)))

(defmethod (setf samplerate) :around (a-samplerate (a-sink sink))
  (when (= 1 (call-next-method))
    (signal 'sink-samplerate-preset-error :samplerate a-samplerate)))

(defmethod (setf number-of-channels) :around (a-number-of-channels (a-sink sink))
  (when (= 1 (call-next-method))
    (signal 'sink-number-of-channels-preset-error :number-of-channels a-number-of-channels)))

(defmethod write-sink ((a-sink sink) (samples float-vector))
  (aubio/bindings::|aubio_sink_do| (internal-aubio-object a-sink) (internal-aubio-object samples) (size samples)))

(defmethod write-sink ((a-sink sink) (samples float-matrix))
  (aubio/bindings::|aubio_sink_do_multi| (internal-aubio-object a-sink) (internal-aubio-object samples) (size samples)))

(defun write-sink (a-sink samples-to-write &key (monophonic t))
  (with-slots (internal-aubio-object) a-sink
    (let ((aubio-write-function (if monophonic #'aubio/bindings::|aubio_sink_do| #'aubio/bindings::|aubio_sink_do_multi|)))
      (funcall aubio-write-function internal-aubio-object (internal-aubio-object samples-to-write) (size samples-to-write)))))

(defmacro with-sink ((variable uri &key (samplerate 0)) &body body)
  `(let ((,variable (make-sink ,uri ,samplerate)))
     (unwind-protect (progn ,@body) (clean ,variable))))
