(in-package :cl-hdf5)

(defclass hdf5 ()
  ((%id :initarg :id
        :reader id)
   (%open :initform t
          :reader is-open
          :writer set-open)))

;; I'm not sure if I like this or a magic variable better
;; for now I'll keep this
(defclass hdf5-sentinel ()
  ())

(defmethod initialize-instance :after ((instance hdf5-sentinel) &rest initargs &key)
  (declare (ignore initargs))
  (let ((id (id instance)))
    (unless (eq (hdf5-type instance) (h5iget-type id))
      ;; If a user puts an ID that isn't of the right type into the object
      ;; we need to close the ID so we don't have a leak.
      ;; But since the type doesn't match the object we don't actually know
      ;; what it is. So fall back to hdf5 itself to figure it out and close.
      (case (h5iget-type id)
        (:h5i-file (h5fclose id))
        (:h5i-dataset (h5dclose id))
        (:h5i-dataspace (h5sclose id))
        (:h5i-datatype (h5tclose id))
        (:h5i-attr (h5aclose id))
        (:h5i-group (h5gclose id)))
      (error 'hdf5-invalid-id :reason (format nil "~a cannot have a ~a identifier."
                                              instance
                                              (h5iget-type id))))))

(defmethod close :before ((instance hdf5) &key abort)
  (declare (ignore abort))
  (unless (< 0 (h5iis-valid (id instance)))
    (error 'hdf5-invalid-id :instance instance)))

(defmethod close :after ((instance hdf5) &key abort)
  (declare (ignore abort))
  (set-open nil instance))
