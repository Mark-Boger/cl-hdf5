;; HDF5 Dataset

(in-package :cl-hdf5)

(defvar +hdf5-types+ (list
                      +h5t-c-s1+))

(defun create-dataset (file-id dataset-name &key (type :scalar) dims maxdims)
  (with-dataspace (space type :dims dims :maxdims maxdims)
    (let ((dset (h5dcreate2 file-id dataset-name +h5t-std-i32be+ space
                            +h5p-default+ +h5p-default+ +h5p-default+)))
      (unless (< 0 dset)
        (error "Error creating dataset"))
      dset)))

(defun write-to-dataset (dataset-id object)
  (cffi:with-foreign-object (data :int 1)
    (setf (cffi:mem-aref data :int) object)
    (h5dwrite dataset-id +h5t-native-int+ +h5s-all+ +h5s-all+ +h5p-default+ data)))

(defun read-from-dataset (dataset-id type))

(wrap-close-function (h5dclose dataset-id) :h5i-dataset)

(defmacro with-dataset ((dset dataset-name file-id &key magic) &body body)
  ;; Honestly this is probably a bad idea but it's fun so I'm doing it anyway
  (labels ((check-form (form func)
             (and (eq (first form) func) (not (symbolp (second form)))))
           (tree-replace (form)
             (cond
               ((atom form) (return-from tree-replace form))
               ((check-form form 'write-to-dataset)
                (return-from tree-replace `(write-to-dataset ,dset ,@(rest form)))))
             (loop :for element :in form
                   :collect (tree-replace element))))
    (let ((modi-body (if magic (tree-replace body) body)))
      (multiple-value-bind (forms decl) (alexandria:parse-body modi-body)
        `(let ((,dset (create-dataset ,file-id ,dataset-name)))
           ,@decl
           (unwind-protect
                (progn ,@forms)
             (close-dataset ,dset)))))))
