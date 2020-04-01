(in-package :cl-aubio/tests)

(def-suite onset-suite)

(in-suite onset-suite)

(def-test silence-threshold (:fixture (detector-fixture #'make-onset-detector))
  (let ((silence-threshold 12.0))
    (setf (silence-threshold detector) silence-threshold)
    (is (silence-threshold detector) silence-threshold)))

(def-test detection-method ()
  (let* ((detection-method :energy)
         (detector (make-onset-detector 512 256 44100 :method detection-method)))
    (is (cffi:foreign-string-to-lisp (slot-value detector 'aubio::internal-method))
        (string-downcase detection-method))
    (clean detector)))

(def-test peak-picking-threshold (:fixture (detector-fixture #'make-onset-detector))
  (let ((peak-picking-threshold 12.0))
    (setf (peak-picking-threshold detector) peak-picking-threshold)
    (is (peak-picking-threshold detector)) peak-picking-threshold))

(def-test adaptive-whitening (:fixture (detector-fixture #'make-onset-detector))
  (is-false (adaptive-whitening-enabled? detector))
  (setf (adaptive-whitening-enabled? detector) t)
  (is-true (adaptive-whitening-enabled? detector)))

(def-test compresssion-factor (:fixture (detector-fixture #'make-onset-detector))
  (let ((compression-factor 72.3))
    (setf (compression-factor detector) compression-factor)
    (is (compression-factor detector) compression-factor)))

(def-test minimum-inter-onset-interval-in-sammples (:fixture (detector-fixture #'make-onset-detector))
  (let ((interval-in-samples 44100))
    (setf (minimum-inter-onset-interval detector) interval-in-samples)
    (is (minimum-inter-onset-interval detector) interval-in-samples)
    (is (minimum-inter-onset-interval detector :unit :seconds) 1.0)
    (is (minimum-inter-onset-interval detector :unit :milliseconds) 1000.0)))

(def-test minimum-inter-onset-interval-in-seconds (:fixture (detector-fixture #'make-onset-detector))
  (let ((sample-rate 44100)
        (interval-in-seconds 2.0))
    (setf (minimum-inter-onset-interval detector :unit :seconds) interval-in-seconds)
    (is (minimum-inter-onset-interval detector :unit :seconds) interval-in-seconds)
    (is (float (minimum-inter-onset-interval detector :unit :samples)) (* sample-rate interval-in-seconds))
    (is (minimum-inter-onset-interval detector :unit :milliseconds) (* 1000 interval-in-seconds))))
