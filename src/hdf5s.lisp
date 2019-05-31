;; HDF5 Dataspaces

(in-package :cl-hdf5)

;; Alias this so we don't have the h5s prefix
(defvar +unlimited+ +h5s-unlimited+)

(defgeneric create-dataspace (dataspace-type &key)
  (:documentation "Create a dataspace of type DATASPACE-TYPE"))

(defmethod create-dataspace ((dataspace (eql :null)) &key)
  (declare (ignore dataspace))
  (h5screate :h5s-null))

(defmethod create-dataspace ((dataspace (eql :scaler)) &key)
  (declare (ignore dataspace))
  (h5screate :h5s-scalar))

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
        (unless (every (lambda (dim maxdim) (<= dim maxdim)) dims maxdims)
          (error "All DIMS must be <= all MAXDIMS")))
      ;; Actually make the dataspace
      (let* ((dims-ptr (alloc rank dims))
             (max-ptr (if maxdims
                          (alloc rank maxdims)
                          +null+))
             (space (h5screate-simple rank dims-ptr max-ptr)))
        (cffi:foreign-free dims-ptr)
        (when maxdims (cffi:foreign-free max-ptr))
        ;; Check to see if we actually made the dataspace
        (when (< space 0)
          (error "Error creating dataspace"))
        space))))

(wrap-close-function (h5sclose space-id) :h5i-dataspace)
