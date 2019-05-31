(in-package :cl-hdf5)

;; This isn't technically in the hdf5f part of the API but _I_ think
;; it makes more sense here.
(defun is-hdf5-file (file-name)
  "Check if a file with the given FILE-NAME is an HDF5 file"
  (let ((is-hdf5 (h5fis-hdf5 (namestring (truename file-name)))))
    (cond
      ((> is-hdf5 0) t)
      ((= is-hdf5 0) nil)
      (t (error "File error")))))

(defun create-hdf5-file (file-name &key (if-exists :error)
                                     (file-creation-property-list :default)
                                     (file-access-property-list :default))
  "Create an HDF5 file with FILE-NAME."
  (declare (ignore file-creation-property-list file-access-property-list))
  (unless (member if-exists '(:error :supersede))
    (error "Invalid if-exists option: ~a" if-exists))
  (format t "File created: ~a~%" file-name))
