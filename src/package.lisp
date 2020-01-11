(defpackage :cl-aubio
  (:use :cl)
  (:nicknames :aubio)
  (:export
     :clean :aubio-to-lisp
     :with-source :do-source :make-source :read-source :channels :duration :samplerate :seek
     :make-float-vector :get-data))
