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

(defmacro define-close ((object) &body body)
  (let ((result (gensym "close-result")))
    `(defmethod hdf5-close (,object)
       (let ((,result (progn ,@body)))
         (if (<= 0 ,result)
             t
             (error 'hdf5-close-error))))))

(defun is-valid (instance)
  (h5iis-valid (id instance)))

(defun validate-or-die (instance)
  (unless (is-valid instance)
    (error 'hdf5-invalid-id :instance instance)))

(defun maybe-propeties (symbol)
  (unless (typep symbol 'properties)
    (error 'type-error :expected-type 'properties
                       :datum symbol))
  (let ((id (id symbol)))
    (if (eql id :default)
        +h5p-default+
        (if (h5iis-valid id)
            id
            (error 'hdf5-invalid-id :instance symbol)))))
