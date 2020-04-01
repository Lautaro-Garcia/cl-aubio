(in-package :cl-aubio/tests)

(def-suite tempo-suite)

(in-suite tempo-suite)

(def-test silence-threshold (:fixture (detector-fixture #'make-tempo-detector))
  (let ((silence-threshold -12.0))
    (setf (silence-threshold detector) silence-threshold)
    (is (silence-threshold detector) silence-threshold)))

(def-test peak-picking-threshold (:fixture (detector-fixture #'make-tempo-detector))
  (let ((peak-picking-threshold 23.4))
    (setf (peak-picking-threshold detector) peak-picking-threshold)
    (is (peak-picking-threshold detector) peak-picking-threshold)))
