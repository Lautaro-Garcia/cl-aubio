(defpackage :cl-aubio
  (:use :cl)
  (:nicknames :aubio)
  (:export
     :clean :aubio-to-lisp :silence-threshold
     :with-source :do-source :make-source :read-source :channels :duration :samplerate :seek
     :make-pitch-detector :detect-pitch :tolerance :pitch-detection-unit :confidence
     :make-onset-detector :reset-onset-detector :peak-picking-threshold :adaptive-whitening-enabled? :adaptive-whitening-enabled :compression-factor :minimum-inter-onset-interval :time-of-latest-onset-detected :detect-onset
     :make-float-vector :get-data :size
     :make-float-matrix :get-channel-data))
