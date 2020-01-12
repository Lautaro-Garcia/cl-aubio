(defpackage :cl-aubio
  (:use :cl)
  (:nicknames :aubio)
  (:export
     :clean :aubio-to-lisp
     :with-source :do-source :make-source :read-source :channels :duration :samplerate :seek
     :make-pitch-detector :detect-pitch :tolerance :pitch-detection-unit :silence-threshold :confidence
     :make-float-vector :get-data :size
     :make-float-matrix :get-channel-data))
