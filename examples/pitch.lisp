(in-package :cl-aubio/examples)

(defun pitch-example ()
  (let* ((total-frames 0)
         (samplerate 44100)
         (pitch-detector (aubio:make-pitch-detector 4096 512 samplerate :method :yin))
         (pitch-vector (make-array 0 :element-type 'aubio:pitch :adjustable t :fill-pointer 0)))

    (setf (aubio:tolerance pitch-detector) 0.8)
    (setf (aubio:pitch-detection-unit pitch-detector) :midi)

    (aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) 256 :sample-rate samplerate :amount-read-variable read)
      (let ((pitch (aubio:detect-pitch pitch-detector samples)))
        (vector-push-extend pitch pitch-vector)
        (format t "~f  ~a  ~a~%" (/ total-frames samplerate) (aubio:pitch pitch) (aubio:confidence pitch))
        (incf total-frames read)))))
