(in-package :cl-aubio)

(defclass aubio-object ()
  ((internal-aubio-object :accessor internal-aubio-object :type cffi:foreign-pointer)))
