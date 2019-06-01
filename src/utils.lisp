(in-package :cl-hdf5)

;; TODO: Make this smarter
(defmacro check-arg (check &optional error)
  (let ((name (second check)))
    `(unless ,check
       ,(if error
            (if (listp error)
                `(error ,@error)
                `(error ,error))
            `(error ,(format nil "Unrecognized ~a option: ~a" name "~a") ,name)))))

(defmacro wrap-close-function (close-function type &optional func-name)
  (let* ((identifier (second close-function))
         (full-type-name (string-downcase (symbol-name type)))
         (actual-type (subseq full-type-name (1+ (position #\- full-type-name))))
         (func (if func-name
                   func-name
                   (intern (string-upcase (format nil "close-~a" actual-type))))))
    `(defun ,func (,identifier)
       (unless (< 0 (h5iis-valid ,identifier))
         (error ,(format nil "Invalid HDF5 ~a identifier" actual-type)))
       (unless (eq (h5iget-type ,identifier) ,type)
         (error ,(format nil "~a is not an HDF5 ~a" identifier actual-type)))
       (if (<= 0 ,close-function)
           t
           (error ,(format nil "Error closing HDF5 ~a" actual-type))))))
