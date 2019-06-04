;; HDF5 Dataset

(in-package :cl-hdf5)

(defun create-dataset (file-id dataset-name &key (type :scalar)
                                              dims maxdims
                                              (datatype-id :int)
                                              (link-creation-property-list :default)
                                              (dataset-creation-property-list :default)
                                              (dataset-access-property-list :default))
  (declare (ignore link-creation-property-list dataset-access-property-list
                   dataset-creation-property-list))
  (check-arg (h5iis-valid file-id))
  (check-arg (member type '(:scalar :null :simple)))
  
  (let ((dtype (or (getf +hdf5-types+ datatype-id) datatype-id)))
    (check-arg (h5iis-valid dtype))
    (with-dataspace (space type :dims dims :maxdims maxdims)
      (let ((dset (h5dcreate2 file-id dataset-name dtype
                              space +h5p-default+ +h5p-default+ +h5p-default+)))
        (unless (< 0 dset)
          (error "Error creating dataset"))
        dset))))

;; TODO: Keyword parsing for options
(defun open-dataset (file-id dataset-name &rest options)
  (declare (ignore options))
  (check-arg (h5iis-valid file-id))
  (let ((dset (h5dopen2 file-id dataset-name +h5p-default+)))
    (unless (< 0 dset)
      (return-from open-dataset (create-dataset file-id dataset-name)))
    dset))

(defun write-to-dataset (dataset-id object)
  (cffi:with-foreign-object (data :int 1)
    (setf (cffi:mem-aref data :int) object)
    (h5dwrite dataset-id +h5t-native-int+ +h5s-all+ +h5s-all+ +h5p-default+ data)))

(defun read-from-dataset (dataset-id type))

(wrap-close-function (h5dclose dataset-id) :h5i-dataset)

(defmacro with-dataset ((dset file-id dataset-name &key magic) &body body)
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
        `(let ((,dset (open-dataset ,file-id ,dataset-name)))
           ,@decl
           (unwind-protect
                (progn ,@forms)
             (close-dataset ,dset)))))))

(defmacro with-datasets ((&rest datasets) &body body)
  (multiple-value-bind (forms decl) (alexandria:parse-body body)
    `(let (,@(loop :for dataset :in datasets
                   :collect `(,(first dataset) (open-dataset ,@(rest dataset)))))
       ,@decl
       (unwind-protect
            (progn ,@forms)
         (progn
           ,@(loop :for dataset :in datasets
                   :collect `(close-dataset ,(first dataset))))))))
