(in-package :cl-aubio/examples)

(defvar *onset-detector* (aubio:make-onset-detector 512 256 44100))

(defvar *onset-vector* (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0))

(defun detect-onsets ()
  (setf (fill-pointer *onset-vector*) 0)
  (aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) 256 :sample-rate 44100)
    (let ((onset (aubio:detect-onset *onset-detector* samples)))
      (when onset (vector-push-extend onset *onset-vector*))))
  (reset-onset-detector *onset-detector*)
  *onset-vector*)
