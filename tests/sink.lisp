(in-package :cl-aubio/tests)

(def-suite sink-suite)

(in-suite sink-suite)

(def-fixture sink-fixture ()
  (let ((sink (make-sink "somewav" 44100)))
    (&body)
    (clean sink)
    (uiop:delete-file-if-exists "somewav")))

(def-test preset-number-of-channels (:fixture sink-fixture)
  (setf (number-of-channels sink) 3)
  (is (= 3 (number-of-channels sink))))

(def-test preset-samplerate (:fixture sink-fixture)
  (let ((sample-rate 22050))
    (setf (samplerate sink) sample-rate)
    (is (= sample-rate (samplerate sink)))))

(def-test preset-samplerate-error (:fixture sink-fixture)
  (let ((erroneous-samplerate 100000000))
    (signals sink-samplerate-preset-error (setf (samplerate sink) erroneous-samplerate))))

(def-test preset-number-of-channels-error (:fixture sink-fixture)
  (signals sink-number-of-channels-preset-error (setf (number-of-channels sink) 0)))
