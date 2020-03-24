(defpackage :cl-aubio
  (:use :cl)
  (:nicknames :aubio)
  (:export
     :clean :aubio-to-lisp :silence-threshold :peak-picking-threshold :confidence :minimum-inter-onset-interval
     :value :unit
     :with-source :do-source :make-source :read-source :channels :duration :samplerate :seek
     :make-pitch-detector :detect-pitch :tolerance :pitch-detection-unit :pitch-detection-method :pitch
     :make-onset-detector :reset-onset-detector :adaptive-whitening-enabled? :compression-factor :time-of-latest-onset-detected :detect-onset :onset-detection-function
     :make-tempo-detector :bpm :time-of-latest-beat-detected :detect-tempo :tempo
     :make-note-detector :detect-note :release-drop :note :midi-note :velocity :note-off :note-on :silence
     :make-float-vector :get-data :size
     :make-float-matrix :get-channel-data))
