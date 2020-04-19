(asdf:defsystem #:cl-aubio
  :description "Aubio bindings for Common Lisp"
  :author "Lautaro García"
  :defsystem-depends-on (:cffi/c2ffi)
  :version "0.0.1"
  :depends-on (:cffi :cffi/c2ffi :cffi-libffi :closer-mop)
  :in-order-to ((test-op (test-op :cl-aubio/tests)))
  :components ((:module "bindings"
                :serial t
                :components ((:file "package")
                             (:module "spec"
                              :components ((:cffi/c2ffi-file "aubio.h"
                                            :c2ffi-executable "/usr/bin/c2ffi"
                                            :package :aubio/bindings
                                            :foreign-library-name "aubio/bindings::aubio"
                                            :include-sources ()
                                            :foreign-library-spec ((t (:or "libaubio.so.5.4.8" "libaubio"))))))))

               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "aubio-mop")
                             (:file "commons")
                             (:file "float-vector")
                             (:file "float-matrix")
                             (:file "timestamp")
                             (:file "source")
                             (:file "pitch")
                             (:file "onset")
                             (:file "tempo")
                             (:file "notes")
                             (:file "filterbank")
                             (:file "sink")))))

(asdf:defsystem #:cl-aubio/tests
  :description "System for the tests of cl-aubio"
  :author "Lautaro García"
  :version "0.0.1"
  :depends-on (:cl-aubio :fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "float-matrix")
               (:file "float-vector")
               (:file "notes")
               (:file "onset")
               (:file "pitch")
               (:file "tempo")
               (:file "filterbank")
               (:file "sink"))
  :perform (test-op (op s) (uiop:symbol-call :5am :run-all-tests)))

(asdf:defsystem #:cl-aubio/examples
  :description "System that contains examples of the usage of cl-aubio"
  :author "Lautaro García"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-aubio :vgplot)
  :pathname "examples/"
  :components ((:file "package")
               (:file "notes")
               (:file "pitch")
               (:file "onset")
               (:file "tempo")
               (:file "filterbank")
               (:file "sink")))
