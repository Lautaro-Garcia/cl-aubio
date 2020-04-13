(in-package :cl-aubio/examples)

(defgeneric get-printable-info (a-note)
  (:documentation "Print a single note"))

(defmethod get-printable-info ((a-note aubio:note-on))
  (list "On" (aubio:midi-note a-note) (aubio:velocity a-note)))

(defmethod get-printable-info ((a-note aubio:note-off))
  (list "Off" (aubio:midi-note a-note) (aubio:velocity a-note)))

(defmethod get-printable-info ((silence (eql nil)))
  (list "Silence" "-" "-"))

(defun print-notes (notes)
  (let ((format-string "~30<~a~;~a~;~a~>~%"))
    (format t format-string "Type" "MIDI note" "Velocity")
    (loop :for note :across notes
          :do (apply #'format (cons t (cons format-string (get-printable-info note)))))))

(defun notes-example ()
  (let ((note-detector (aubio:make-note-detector 512 256 44100))
        (note-vector (make-array 0 :element-type 'aubio:note :adjustable t :fill-pointer 0)))

    (setf (aubio:silence-threshold note-detector) -90.0)

    (aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) 256 :sample-rate 44100)
      (let ((note (aubio:detect-note note-detector samples)))
        (when note (vector-push-extend note note-vector))))
    (print-notes note-vector)))
