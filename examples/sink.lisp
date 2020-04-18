(in-package :cl-aubio/examples)

(defun sink-example (&key (poliphonic nil))
  (let ((hop-size 256))
    (aubio:with-sink (sink "G-major-tune-copy.wav" :samplerate 44100)
      (aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) hop-size :sample-rate 44100 :monophonic (not poliphonic))
        (aubio:write-sink sink samples)))))
