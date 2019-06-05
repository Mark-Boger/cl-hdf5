(in-package :cl-hdf5)

(defun simple-hdf5-reporter (condition stream)
  (format stream "~a" (reason condition)))

(define-condition hdf5-error (error)
  ((%reason :initarg :reason
            :reader reason))
  (:report simple-hdf5-reporter))

(define-condition hdf5-invalid-id (hdf5-error)
  ((%hdf5-instance :initarg :instance
                   :reader instance))
  (:report (lambda (condition stream)
             (format stream "~a does not have a valid HDF5 id." (instance condition)))))

(define-condition hdf5-close-error (hdf5-error)
  ())

(define-condition hdf5-file-error (hdf5-error)
  ())

(define-condition hdf5-file-creation-error (hdf5-file-error)
  ((%filename :initarg filename
              :reader filename))
  (:report (lambda (condition stream)
             (format stream "Unable to create HDF5 file at ~a." (filename condition)))))

(define-condition hdf5-file-open-error (hdf5-file-error)
  ())

(define-condition dataspace-error (hdf5-error)
  ())

(define-condition dataspace-size-mismatch (dataspace-error)
  ((%expected-size :initarg :expected-size
                   :reader expected-size)
   (%actual-size :initarg :actual-size
                 :reader actual-size))
  (:report (lambda (condition stream)
             (format stream "Expected ~d elements but got ~d."
                     (expected-size condition) (actual-size condition)))))

(define-condition dataspace-creation-error (dataspace-error)
  ())

(define-condition dataset-error (hdf5-error)
  ())

(define-condition dataset-creation-error (dataset-error)
  ())

(define-condition dataset-invalid-dataspace (dataset-error)
  ())
