(in-package :cl-hdf5)

(define-condition hdf5-error (error)
  ())

(define-condition hdf5f-invalid-id (hdf5-error)
  ())

(define-condition hdf5-file-error (hdf5-error)
  ())

(define-condition hdf5-file-creation-error (hdf5-file-error)
  ())

(define-condition hdf5-file-open-error (hdf5-file-error)
  ())

(define-condition dataspace-error (hdf5-error)
  ())

(define-condition dataspace-creation-error (dataspace-error)
  ())
