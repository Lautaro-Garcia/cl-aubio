(in-package :cl-aubio/tests)

(def-suite float-vector)

(in-suite float-vector)

(def-fixture with-float-vector ()
  (let ((vector (make-float-vector-from-lisp-vector #(1.0 2.0 3.0))))
    (&body)
    (clean vector)))

(defun every-element-of-vector-is (float-vector element)
  (let ((lisp-vector (aubio-to-lisp float-vector)))
    (is (equalp lisp-vector (make-array (length lisp-vector) :initial-element element)))))

(def-test create-vector-from-lisp-vector ()
  (let* ((original-vector #(1.0 2.0 3.0))
         (float-vector (make-float-vector-from-lisp-vector original-vector)))
    (is (equalp (aubio-to-lisp float-vector) original-vector))
    (clean float-vector)))

(def-test setting-and-getting-samples (:fixture with-float-vector)
  (setf (get-vector-sample vector 0) 42.0)
  (is (get-vector-sample vector 0) 42.0))

(def-test error-vector-index-is-an-integer (:fixture with-float-vector)
  (signals type-error (get-vector-sample vector 1.0))
  (signals type-error (get-vector-sample vector "index")))

(def-test error-vector-contains-float-elements ()
  (signals type-error (make-float-vector #(1 2 3)))
  (signals type-error (make-float-vector #("one" "two" "three"))))

(def-test set-all-elements-to-a-value (:fixture with-float-vector)
  (set-all-samples vector 10.0)
  (every-element-of-vector-is vector 10.0))

(def-test set-all-elements-to-zero (:fixture with-float-vector)
  (set-all-samples-to-zero vector)
  (every-element-of-vector-is vector 0))

(def-test set-all-elements-to-one (:fixture with-float-vector)
  (set-all-samples-to-one vector)
  (every-element-of-vector-is vector 1.0))

(def-test reverse-vector (:fixture with-float-vector)
  (is (equalp (aubio-to-lisp (reverse-vector vector))
              #(3.0 2.0 1.0))))

(def-test all-elements-scaled (:fixture with-float-vector)
  (is (equalp (aubio-to-lisp (weight-vector vector #(5.0 3.0 2.0)))
              #(5.0 6.0 6.0))))

(def-test copy-vector (:fixture with-float-vector)
  (let ((copied-vector (copy vector)))
    (is (equalp (aubio-to-lisp vector)
                (aubio-to-lisp copied-vector)))
    (clean copied-vector)))

(def-test weighted-copy (:fixture with-float-vector)
  (let ((copied-vector (weighted-copy vector #(5.0 3.0 2.0))))
    (is (equalp (aubio-to-lisp copied-vector)
                #(5.0 6.0 6.0)))
    (is (not (eql vector copied-vector)))
    (clean copied-vector)))
