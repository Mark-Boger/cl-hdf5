;; HDF5 Dataspaces

(in-package :cl-hdf5)

(defclass dataspace (hdf5-sentinel hdf5)
  ((%hdf5-type :initform :h5i-dataspace
               :reader hdf5-type
               :allocation :class)))

(defclass null-dataspace (dataspace)
  ())

(defclass ranked-dataspace (dataspace)
  ((%rank :reader rank)
   (%dims :initarg :dims
          :initform '(1)
          :reader dims)
   (%space :reader total-space)))

(defclass scalar-dataspace (ranked-dataspace)
  ())

(defclass simple-dataspace (ranked-dataspace)
  ((%maxdims :initarg :max-dims
             :reader max-dims)))

(defmethod initialize-instance :after ((instance ranked-dataspace) &rest initargs &key)
  (declare (ignore initargs))
  (let ((dims (dims instance)))
    (with-slots ((space %space) (rank %rank)) instance
      (setf space (reduce #'* dims))
      (setf rank (length dims)))))

(defmethod describe-object ((dataspace dataspace) stream)
  (format stream "~a is a~:[ closed~;n open~] dataspace" dataspace
          (is-open dataspace)))

(defmethod describe-object :after ((dataspace simple-dataspace) stream)
  (let ((max-dims (max-dims dataspace)))
    (when (is-open dataspace)
      (setf max-dims (mapcar (lambda (dim)
                               (when (= dim +unlimited+)
                                 '+unlimited+))
                             max-dims))
      (format stream "~&  it has dimensions of ~a~:[~;~&  and maximum dimensions ~:*~a ~]"
              (dims dataspace) max-dims))))

(defun make-null-dataspace (dataspace-id)
  (make-instance 'null-dataspace :id dataspace-id))

(defun make-scalar-dataspace (dataspace-id)
  (make-instance 'scalar-dataspace :id dataspace-id))

(defun make-simple-dataspace (dataspace-id dims max-dims)
  (make-instance 'simple-dataspace :id dataspace-id
                                   :dims dims
                                   :max-dims max-dims))

;; Alias this so we don't have the h5s prefix
(defvar +unlimited+ +h5s-unlimited+)

(defgeneric create-dataspace (dataspace-type &key)
  (:documentation
   "Create a dataspace of type DATASPACE-TYPE.
Where DATASPACE-TYPE is one of :null :scalar or :simple."))

;; I feel like this is wrong
(defmethod create-dataspace ((dataspace (eql :null)) &key &allow-other-keys)
  (declare (ignore dataspace))
  (make-null-dataspace (create-or-die (h5screate :h5s-null) dataspace-creation-error)))

(defmethod create-dataspace ((dataspace (eql :scalar)) &key &allow-other-keys)
  (declare (ignore dataspace))
  (make-scalar-dataspace (create-or-die (h5screate :h5s-scalar) dataspace-creation-error)))

(defmethod create-dataspace ((dataspace (eql :simple)) &key dims maxdims)
  (declare (ignore dataspace))
  (check-arg (consp dims) "To create a simple dataspace DIMS must be a list")
  ;; Checking listp allows maxdims to be NIL
  (check-arg (listp maxdims) "To create a simple dataspace MAXDIMS must be a list")

  (let ((rank (list-length dims))
        (rank-max (list-length maxdims)))
    (when (and maxdims (not (= rank rank-max)))
      (error "Rank mismatch in simple dataspace creation ~a != ~a" rank rank-max))
    (when (or (< rank 1) (> rank 32))
      (error "Dataspace ranke must be between 1 and 32"))
    (flet ((is-valid-dim (dim)
             (and (integerp dim) (> dim 0) (<= dim +unlimited+)))
           (alloc (size content &optional (type 'hsize-t))
             (cffi:foreign-alloc type :count size :initial-contents content))
           (process-symbols (dim)
             (if (symbolp dim) (symbol-value dim) dim)))
      ;; Process any symbols that in DIMS
      (setf dims (mapcar #'process-symbols dims))
      (unless (every #'is-valid-dim dims)
        (error "Every element in DIMS must be a positive integer"))
      (when maxdims
        ;; Process any symbols in MAXDIMS
        (setf maxdims (mapcar #'process-symbols maxdims))
        (unless (every #'is-valid-dim  maxdims)
          (error "Every element in MAXDIMS must be either a positive integer or +UNLIMITED+"))
        (unless (every #'<= dims maxdims)
          (error "All DIMS must be <= all MAXDIMS")))
      ;; Actually make the dataspace
      (let* ((dims-ptr (alloc rank dims))
             (max-ptr (if maxdims
                          (alloc rank maxdims)
                          +null+))
             (space (create-or-die (h5screate-simple rank dims-ptr max-ptr)
                                   dataspace-creation-error)))
        (cffi:foreign-free dims-ptr)
        (when maxdims (cffi:foreign-free max-ptr))
        ;; Check to see if we actually made the dataspace
        (make-simple-dataspace space dims maxdims)))))

(define-close ((instance dataspace))
  (h5sclose (id instance)))

(defmacro with-dataspace ((space-name type &key (dims '(1)) maxdims) &body body)
  (multiple-value-bind (forms decls) (alexandria:parse-body body)
    `(let ((,space-name (create-dataspace ,type :dims ,dims :maxdims ,maxdims)))
       ,@decls
       (unwind-protect
            (progn ,@forms)
         (hdf5-close ,space-name)))))
