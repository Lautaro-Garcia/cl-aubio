(in-package :cl-aubio)

(defgeneric clean (an-aubio-object)
  (:documentation "Cleans the memory allocated for AN-AUBIO-OBJECT."))

(defgeneric aubio-to-lisp (an-aubio-object)
  (:documentation "Transforms an aubio data structure to a lisp one."))

(defgeneric get-data (an-aubio-object)
  (:documentation "Obtains the data inside an aubio data structure."))

(defgeneric silence-threshold (an-aubio-detector)
  (:documentation "Get the detector silence threshold"))

(defgeneric (setf silence-threshold) (a-silence-threshold an-aubio-detector)
  (:documentation "Set the detector's silence threshold"))
