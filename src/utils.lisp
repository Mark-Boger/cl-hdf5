(in-package :cl-hdf5)

;; TODO: Make this smarter
(defmacro check-arg (check)
  (let ((name (second check)))
    `(unless ,check
       (error ,(format nil "Unrecognized ~a option: ~a" name "~a") ,name))))
