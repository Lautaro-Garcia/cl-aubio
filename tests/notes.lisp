(in-package :cl-aubio/tests)

(def-suite notes-suite)

(in-suite notes-suite)

(def-test silence-threshold (:fixture (detector-fixture #'make-note-detector))
  (let ((silence-threshold -10.0))
    (setf (silence-threshold detector) silence-threshold)
    (is (silence-threshold detector) silence-threshold)))

(def-test minimum-inter-onset-interval (:fixture (detector-fixture #'make-note-detector))
  (let ((interval 2000.0))
    (setf (minimum-inter-onset-interval-ms detector) interval)
    (is (minimum-inter-onset-interval-ms detector) interval)))

(def-test release-drop (:fixture (detector-fixture #'make-note-detector))
  (let ((release-drop 10.0))
    (setf (release-drop detector) release-drop)
    (is (release-drop detector) release-drop)))
