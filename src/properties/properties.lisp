;; HDF5 Properties

(in-package :cl-hdf5)

(defclass property-list (hdf5)
  ((%name :initarg :name
          :initform nil
          :reader name)
   (%base :initarg :base
          :initform nil
          :reader base)))

(defclass creation-property-list (property-list)
  ()
  (:documentation "Creation properties control permanent object characteristcs. These 
properties are established when an object is created and cannot be changed."))

(defclass access-property-list (property-list)
  ()
  (:documentation
   "Access properties control transient object characteristics, may change with when an
 object is accessed."))

(defclass transfer-property-list (property-list)
  ()
  (:documentation
   "Transfer properties control transient I/O characteristcs, may change when data is
accessed"))

(defmethod initialize-instance :after ((instance property-list) &rest initargs &key)
  (declare (ignore initargs))
  (unless (name instance)
    (setf (slot-value instance '%name) (gensym (format nil "~a-" (base instance))))))

(defmethod describe-object ((object property-list) stream)
  (format stream "~a is a property list~:[~;~& named ~:*~a~]~:[~;~& based on ~:*~a~]"
          object
          (name object)
          (base object)))


(defun make-property-list (property-id &key (type 'property-list) name base)
  (make-instance type :id property-id :name name :base (when base
                                                         (name base))))

(defun create-property-list (base-property-list &optional name)
  (make-property-list (h5pcreate (id base-property-list)) :base base-property-list
                                                          :name name
                                                          :type (type-of base-property-list)))

(define-close ((instance property-list))
  (h5pclose (id instance)))
