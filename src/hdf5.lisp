(in-package :cl-hdf5)

(defclass hdf5 ()
  ((%id :initarg :id
        :reader id)
   (%open :initform t
          :reader is-open
          :writer set-open)))
