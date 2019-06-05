;; HDF5 Properties

(in-package :cl-hdf5)

(defclass properties (hdf5)
  ())

(defun make-default-property-list ()
  (make-instance 'properties :id :default))

(defvar +default-properties+ (make-default-property-list))
