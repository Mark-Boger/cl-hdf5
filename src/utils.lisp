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

(defmacro create-or-die (creation-fn error)
  (let ((object (gensym "object")))
    `(let ((,object (,@creation-fn)))
       (unless (< 0 ,object)
         ;; Should probably make this actually take args
         (error ',error))
       ,object)))

(defmacro wrap-close-function (close-function type &optional func-name)
  (let* ((ident (gensym "idetifier"))
         (identifier (second close-function))
         (full-type-name (string-downcase (symbol-name type)))
         (actual-type (subseq full-type-name (1+ (position #\- full-type-name))))
         (func (if func-name
                   func-name
                   (intern (string-upcase (format nil "close-~a" actual-type))))))
    (setf (nth 1 close-function) ident)
    `(defun ,func (,identifier)
       (let ((,ident (id ,identifier)))
         (unless (< 0 (h5iis-valid ,ident))
           (error ,(format nil "Invalid HDF5 ~a identifier" actual-type)))
         (unless (eq (h5iget-type ,ident) ,type)
           (error ,(format nil "~a is not an HDF5 ~a" identifier actual-type)))
         (if (<= 0 ,close-function)
             (prog1 t
               (set-open nil ,identifier))
             (error ,(format nil "Error closing HDF5 ~a" actual-type)))))))

(defun maybe-propeties (symbol)
  (if (eql symbol :default)
      +h5p-default+
      (if (h5iis-valid symbol)
          symbol
          (error 'invalid-hdf5-id))))
