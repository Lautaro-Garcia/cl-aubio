(in-package :cl-aubio/examples)

(defun tempo-example ()
  (let* ((total-frames 0)
         (samplerate 44100)
         (hop-size 256)
         (tempo-detector (aubio:make-tempo-detector 512 hop-size samplerate))
         ;;tempo detection delay, in samples
          ;;default to 4 blocks delay to catch up with)
         (delay (* 4 hop-size)))

    (aubio:do-source (samples (namestring (truename "../examples/G-major-tune.wav")) hop-size :amount-read-variable read)
      (let ((tempo (aubio:detect-tempo  tempo-detector samples)))
        (when tempo
          (format t "~f~%" (/ (+ (- total-frames delay)
                                 (* (aubio:value (aubio:last-beat-position tempo)) hop-size))
                              samplerate))))
      (incf total-frames read))))
