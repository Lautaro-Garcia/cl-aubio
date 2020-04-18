(in-package :cl-aubio/tests)

(def-suite float-matrix)

(in-suite float-matrix)

(def-fixture with-float-matrix ()
  (let ((float-matrix (make-float-matrix-from-array #2A((1.0 2.0 3.0) (4.0 5.0 6.0)))))
    (&body)
    (clean float-matrix)))

(defun every-element-of-matrix-is (float-matrix value)
  (let ((lisp-array (aubio-to-lisp float-matrix)))
    (is (equalp lisp-array (make-array (array-dimensions lisp-array) :initial-element value)))))

(def-test from-lisp-multidimensional-array ()
  (let* ((array #2A((1.0 2.0 3.0) (4.0 5.0 6.0)))
         (float-matrix (make-float-matrix-from-array array)))
    (is (width float-matrix) 3)
    (is (height float-matrix) 2)
    (is (equalp (aubio-to-lisp float-matrix) array))
    (clean float-matrix)))

(def-test error-making-a-matrix-from-a-vector ()
  (let ((vector #(1.0 2.0 3.0)))
    (signals type-error (make-float-matrix-from-array vector))))

(def-test error-making-a-matrix-from-a-2-dimensions-array-with-integers ()
  (let ((array-of-integers #2((1 2 3) (4 5 6))))
    (signals type-error (make-float-matrix-from-array array-of-integers))))

(def-test setting-and-getting-samples (:fixture with-float-matrix)
  (setf (get-sample float-matrix 1 1) 2.0)
  (is (get-sample float-matrix 1 1)   2.0))

(def-test error-matrix-uses-integers-as-indexes (:fixture with-float-matrix)
  (signals type-error (setf (get-sample float-matrix 2.0 3.0) 8.0))
  (signals type-error (setf (get-sample float-matrix "index" "index") 12.5))
  (signals type-error (get-sample float-matrix 1.0 1))
  (signals type-error (get-sample float-matrix "index" 1.0)))

(def-test error-matrix-uses-floats-as-samples (:fixture with-float-matrix)
  (signals type-error (setf (get-sample float-matrix 1 1) 42))
  (signals type-error (setf (get-sample float-matrix 1 1) "sample")))

(def-test channel-data (:fixture with-float-matrix)
  (let ((first-channel (get-channel-data float-matrix 0 :as-lisp-vector t)))
    (is (equalp first-channel #(1.0 2.0 3.0)))))

(def-test set-all-elements-to-a-value (:fixture with-float-matrix)
  (set-all-samples float-matrix 42.0)
  (every-element-of-matrix-is float-matrix 42.0))

(def-test set-all-elements-to-zero ()
  (let ((float-matrix (make-float-matrix 5 5)))
    (set-all-samples-to-zero float-matrix)
    (every-element-of-matrix-is float-matrix 0)
    (clean float-matrix)))

(def-test set-all-elements-to-ones (:fixture with-float-matrix)
  (set-all-samples-to-one float-matrix)
  (every-element-of-matrix-is float-matrix 1.0))

(def-test all-elements-reversed (:fixture with-float-matrix)
  (is (equalp (aubio-to-lisp (reverse-matrix float-matrix))
              #2A((3.0 2.0 1.0) (6.0 5.0 4.0)))))

(def-test all-elements-scaled (:fixture with-float-matrix)
  (is (equalp (aubio-to-lisp (weight-matrix float-matrix 2.0))
              #2A((2.0 4.0 6.0) (8.0 10.0 12.0)))))

(def-test all-elements-scaled-by-vector (:fixture with-float-matrix)
  (is (equalp (aubio-to-lisp (weight-matrix float-matrix #(2.0 3.0 2.0)))
              #2A((2.0 6.0 6.0) (8.0 15.0 12.0)))))

(def-test scale-a-row-by-a-factor (:fixture with-float-matrix)
  (is (equalp (aubio-to-lisp (weight-matrix-row float-matrix 1 2))
              #2A((1.0 2.0 3.0) (8.0 10.0 12.0)))))

(def-test copy-matrix (:fixture with-float-matrix)
  (let ((copy (copy float-matrix)))
    (is (equalp (aubio-to-lisp float-matrix)
                (aubio-to-lisp copy)))
    (clean copy)))

(def-test copy-matrix-to-another-memory-address (:fixture with-float-matrix)
  (let ((another-float-matrix (make-float-matrix (width float-matrix) (height float-matrix))))
    (copy float-matrix :to another-float-matrix)
    (is (equalp (aubio-to-lisp float-matrix)
                (aubio-to-lisp another-float-matrix)))
    (clean another-float-matrix)))

(def-test multiply-matrix-with-vector (:fixture with-float-matrix)
  (let* ((vector (make-float-vector-from-lisp-vector #(2.0 3.0 2.0)))
         (result (multiply-float-matrix-with-vector float-matrix vector)))
    (is (equalp (aubio-to-lisp result) #(14.0 35.0)))
    (clean vector)
    (clean result)))

(def-test total-size (:fixture with-float-matrix)
  (is (= (size float-matrix) 6)))
