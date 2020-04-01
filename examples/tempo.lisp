(in-package :cl-aubio/examples)

(defparameter *sample-rate* 44100)
(defparameter *hop-size* 256)
(defvar *tempo-detector* (aubio:make-tempo-detector 512 *hop-size* *sample-rate*))

;;tempo detection delay, in samples
;;default to 4 blocks delay to catch up with)
(defparameter *delay* (* 4 *hop-size*))


(defun detect-tempos ()
  (setf (fill-pointer *tempo-vector*) 0)
  (let ((total-frames 0))
    (aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) *hop-size* :amount-read-variable read)
      (let ((tempo (aubio:detect-tempo  *tempo-detector* samples)))
        (when tempo
          (format t "~f~%" (/ (+ (- total-frames *delay*)
                                 (* (aubio:value (aubio:last-beat-position tempo)) *hop-size*))
                              *sample-rate*))))
      (incf total-frames read))))
