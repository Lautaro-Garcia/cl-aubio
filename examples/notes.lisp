(in-package :cl-aubio/examples)

(defvar *note-detector* (aubio:make-note-detector 512 256 44100))

(setf (aubio:silence-threshold *note-detector*) -90.0)

(defvar *note-vector* (make-array 0 :element-type 'note :adjustable t :fill-pointer 0))

(defparameter *format-string* "~30<~a~;~a~;~a~>~%")

(defgeneric get-printable-info (a-note)
  (:documentation "Print a single note"))

(defmethod get-printable-info ((a-note note-on))
  (list "On" (midi-note a-note) (velocity a-note)))

(defmethod get-printable-info ((a-note note-off))
  (list "Off" (midi-note a-note) (velocity a-note)))

(defmethod get-printable-info ((silence (eql nil)))
  (list "Silence" "-" "-"))

(defun print-notes (notes)
  (format t *format-string* "Type" "MIDI note" "Velocity")
  (loop :for note :across notes
        :do (apply #'format (cons t (cons *format-string* (get-printable-info note))))))

(defun detect-notes ()
  (setf (fill-pointer *note-vector*) 0)
  (cl-aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) 256 :sample-rate 44100)
    (let ((note (aubio:detect-note *note-detector* samples)))
      (when note (vector-push-extend note *note-vector*))))
  (print-notes *note-vector*))
