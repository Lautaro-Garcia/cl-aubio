(in-package :cl-aubio/tests)

(def-fixture detector-fixture (constructor)
  "Create and clean a detector Aubio object"
  (let ((detector (funcall constructor 512 256 44100)))
    (&body)
    (clean detector)))
