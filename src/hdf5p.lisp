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


;; I could make something to give everything a name automatically
;; and I probably should (debatable), but I don't feel like it. So I won't

;; Default
(defvar +default-property-list+ (make-property-list :default :name 'default))

;; File property-list
(defvar +file-create+ (make-property-list +h5p-file-create+ :name 'file-create
                                                            :type 'creation-property-list))
(defvar +file-access+ (make-property-list +h5p-file-access+ :name 'file-access
                                                            :type 'access-property-list))

;; According the specification https://support.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Mount
;; there aren't any actual file mount properties and the proper value to pass is
;; H5P_DEFAULT so for now this is an alias to +default-property-list+
(defvar +file-mount+ +default-property-list+)

;; Object property-list
(defvar +object-create+ (make-property-list +h5p-object-create+ :name 'object-create))
(defvar +object-copy+ (make-property-list +h5p-object-copy+     :name 'object-copy))

;; Group property-list
(defvar +group-create+ (make-property-list +h5p-group-create+ :name 'group-create
                                                              :type 'creation-property-list))
(defvar +group-access+ (make-property-list +h5p-group-access+ :name 'group-access
                                                              :type 'access-property-list))

;; Link property-list
(defvar +link-create+ (make-property-list +h5p-link-create+ :name 'link-create
                                                            :type 'creation-property-list))
(defvar +link-access+ (make-property-list +h5p-link-access+ :name 'link-access
                                                            :type 'access-property-list))

;; Dataset property-list
(defvar +dataset-create+
  (make-property-list +h5p-dataset-create+ :name 'dataset-create
                                           :type 'creation-property-list))
(defvar +dataset-access+
  (make-property-list +h5p-dataset-access+ :name 'dataset-access
                                           :type 'access-property-list))
(defvar +dataset-transfer+
  (make-property-list +h5p-dataset-xfer+ :name 'dataset-transfer
                                         :type 'transfer-property-list))

;; Datatype property-list
(defvar +datatype-create+
  (make-property-list +h5p-datatype-create+ :name 'datatype-create
                                            :type 'creation-property-list))
(defvar +datatype-access+
  (make-property-list +h5p-datatype-access+ :name 'datatype-access
                                            :type 'access-property-list))

;; String property-list
(defvar +string-create+
  (make-property-list +h5p-string-create+ :name 'string-create
                                          :type 'creation-property-list))

;; Attribute property-list
(defvar +attribute-create+ (make-property-list +h5p-attribute-create+
                                               :name 'attribute-create
                                               :type 'creation-property-list))

(defun create-property-list (base-property-list &optional name)
  (make-property-list (h5pcreate (id base-property-list)) :base base-property-list
                                                          :name name
                                                          :type (type-of base-property-list)))

(define-close ((instance property-list))
  (h5pclose (id instance)))
