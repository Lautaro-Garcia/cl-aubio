(in-package :cl-aubio/tests)

(def-suite filterbank-suite)

(in-suite filterbank-suite)

(def-fixture filterbank-fixture ()
  "Create and clean a filterbank Aubio object"
  (let ((filterbank (make-filterbank 2 512)))
    (&body)
    (clean filterbank)))

(def-test normalization (:fixture filterbank-fixture)
  (is (normalization-enabled? filterbank) nil)
  (setf (normalization-enabled? filterbank) t)
  (is (normalization-enabled? filterbank) t))

(def-test power (:fixture filterbank-fixture)
  (let ((power 2.3))
    (is (power-parameter filterbank) 1.0)
    (setf (power-parameter filterbank) power)
    (is (power-parameter filterbank) power)))

(def-test set-filter-coefficients (:fixture filterbank-fixture)
  (let ((coefficients (make-float-matrix 257 2)))
    (set-all-samples coefficients 42.0)
    (setf (filter-coefficients filterbank) coefficients)
    (is (equalp (aubio-to-lisp (filter-coefficients filterbank))
                (aubio-to-lisp coefficients)))
    (clean coefficients)))
