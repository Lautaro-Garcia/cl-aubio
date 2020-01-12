(asdf:defsystem #:cl-aubio
  :description "Aubio bindings for Common Lisp"
  :author "Lautaro García"
  :version "0.0.1"
  :pathname "src"
  :depends-on (:cl-aubio/raw-bindings)
  :components ((:file "package")
               (:file "commons")
               (:file "float-vector")
               (:file "float-matrix")
               (:file "source")
               (:file "pitch")))


(asdf:defsystem #:cl-aubio/raw-bindings
  :description "System that contains the raw C bindings for Aubio"
  :author "Lautaro García"
  :version "0.4.9"
  :defsystem-depends-on (:cffi/c2ffi)
  :serial t
  :depends-on (:cffi :cffi/c2ffi :cffi-libffi)
  :pathname "bindings/"
  :components ((:file "package")
               (:module "spec" :components ((:cffi/c2ffi-file "aubio.h"
                                             :c2ffi-executable "/usr/bin/c2ffi"
                                             :package :aubio/bindings
                                             :foreign-library-name "aubio/bindings::aubio"
                                             :include-sources ()
                                             :foreign-library-spec ((t (:or "libaubio.so.5.4.8" "libaubio"))))))))
