(in-package :cl-aubio/tests)

(def-suite pitch-suite)

(in-suite pitch-suite)

(def-test tolerance (:fixture (detector-fixture #'make-pitch-detector))
  (let ((tolerance 42.0))
    (setf (tolerance detector) tolerance)
    (is (tolerance detector)  tolerance)))

(def-test silence-threshold (:fixture (detector-fixture #'make-pitch-detector))
  (let ((silence-threshold -33.3))
    (setf (silence-threshold detector) silence-threshold)
    (is (silence-threshold detector) silence-threshold)))

(def-test detection-unit (:fixture (detector-fixture #'make-pitch-detector))
  (let ((detection-unit :midi))
    (setf (pitch-detection-unit detector) detection-unit)
    (is (pitch-detection-unit detector) (string-downcase detection-unit))))
