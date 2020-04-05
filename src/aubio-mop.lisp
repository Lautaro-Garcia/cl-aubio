(in-package :cl-aubio)

(defclass aubio-class (standard-class) ())

(defmethod c2mop:validate-superclass ((class aubio-class) (superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass aubio-class))
  t)

(defclass aubio-object ()
  ((internal-aubio-object :accessor internal-aubio-object :type cffi:foreign-pointer))
  (:metaclass aubio-class))

(defclass aubio-direct-slot-definition (c2mop:standard-direct-slot-definition)
  ((aubio-reader :initform nil :initarg :aubio-reader :accessor aubio-reader)
   (aubio-writer :initform nil :initarg :aubio-writer :accessor aubio-writer)))

(defmethod aubio-reader ((direct-slot-definition c2mop:standard-direct-slot-definition))
  nil)

(defmethod aubio-writer ((direct-slot-definition c2mop:standard-direct-slot-definition))
  nil)

(defun is-aubio-direct-initarg? (initargs)
  (or (getf initargs :aubio-reader)
      (getf initargs :aubio-writer)))

(defmethod c2mop:direct-slot-definition-class ((class aubio-class) &rest initargs)
  (if (is-aubio-direct-initarg? initargs)
      (find-class 'aubio-direct-slot-definition)
      (find-class 'c2mop:standard-direct-slot-definition)))

(defclass aubio-effective-slot-definition (c2mop:standard-effective-slot-definition)
  ((aubio-reader :initform nil :initarg :aubio-reader :accessor aubio-reader)
   (aubio-writer :initform nil :initarg :aubio-writer :accessor aubio-writer)))

(defgeneric add-aubio-accessors (effective-slot-definition direct-slots))

(defmethod add-aubio-accessors (effective-slot-definition direct-slots) ())

(defmethod add-aubio-accessors ((effective-slot-definition  aubio-effective-slot-definition) direct-slots)
  (let ((slot-with-aubio-reader (find-if #'aubio-reader direct-slots))
        (slot-with-aubio-writer (find-if #'aubio-writer direct-slots)))
    (when slot-with-aubio-reader
      (setf (aubio-reader effective-slot-definition) (aubio-reader slot-with-aubio-reader)))
    (when slot-with-aubio-writer
      (setf (aubio-writer effective-slot-definition) (aubio-writer slot-with-aubio-writer)))))

(defmethod c2mop:compute-effective-slot-definition ((class aubio-class) name direct-slots)
  (let ((effective-slot-definition (call-next-method)))
    (add-aubio-accessors effective-slot-definition direct-slots)
    effective-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class aubio-class) &rest initargs)
  (let* ((slot-name (getf initargs :name))
         (direct-slots (c2mop:class-direct-slots class))
         (direct-slot (find-if (lambda (direct-slot-definition) (eq (c2mop:slot-definition-name direct-slot-definition)
                                                                  slot-name))
                               direct-slots))
         (is-aubio? (and direct-slot (or (aubio-reader direct-slot) (aubio-writer direct-slot)))))
    (if is-aubio?
        (find-class 'aubio-effective-slot-definition)
        (find-class 'c2mop:standard-effective-slot-definition))))

(defmethod c2mop:slot-value-using-class ((class aubio-class) object (slot-definition aubio-effective-slot-definition))
  (let* ((aubio-reader (aubio-reader slot-definition))
         (binding (find-symbol (symbol-name aubio-reader) 'aubio/bindings)))
    (if binding
        (funcall binding (internal-aubio-object object))
        (error "The binding ~a does not exist in the Aubio API" aubio-reader))))

(defmethod (setf c2mop:slot-value-using-class) (new-value (class aubio-class) object (slot-definition aubio-effective-slot-definition))
  (let* ((aubio-writer (aubio-writer slot-definition))
         (binding (find-symbol (symbol-name aubio-writer) 'aubio/bindings)))
    (if binding
        (funcall binding (internal-aubio-object object) new-value)
        (error "The binding ~a does not exist in the Aubio API" aubio-writer))))
