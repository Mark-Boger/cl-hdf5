;; HDF5 Dataset

(in-package :cl-hdf5)

(defclass dataset (hdf5-sentinel hdf5)
  ((%name :initarg :name
          :accessor name)
   (%type :initarg :type
          :accessor data-type)
   (%file :initarg :file
          :reader file)
   (%space :initarg :space
           :accessor dataspace)
   (%hdf5-type :initform :h5i-dataset
               :reader hdf5-type
               :allocation :class)))

(defmethod describe-object ((dataset dataset) stream)
  (format stream "~&~a is a~:[ closed~;n open~] dataset named \"~a\". 
  It's attached to ~a
  It has a type of ~a"
          dataset
          (data-type dataset)
          (name dataset)
          (filename (file dataset))
          (data-type dataset)))

(defun make-dataset (dataset-id file name type dataspace)
  (make-instance 'dataset :id dataset-id :file file :name name :type type :space dataspace))

(defun create-dataset (file dataset-name &key (type :scalar)
                                           dims maxdims
                                           (datatype-id :int)
                                           (link-creation-property-list +default-properties+)
                                           (dataset-creation-property-list
                                            +default-properties+)
                                           (dataset-access-property-list
                                            +default-properties+))
  (validate-or-die file)
  (check-arg (member type '(:scalar :null :simple)))
  
  (let ((dtype (or (getf +hdf5-types+ datatype-id) datatype-id))
        (link-creation-list (maybe-propeties link-creation-property-list))
        (dataset-creation-list (maybe-propeties dataset-creation-property-list))
        (dataset-access-list (maybe-propeties dataset-access-property-list)))
    (validate-or-die dtype)
    (with-dataspace (space type :dims dims :maxdims maxdims)
      (make-dataset (create-or-die (h5dcreate2 (id file) dataset-name (id dtype) (id space)
                                               link-creation-list dataset-creation-list
                                               dataset-access-list)
                                   dataset-creation-error)
                    file dataset-name dtype space))))

(defun open-dataset (file dataset-name &rest options)
  (declare (ignore options))
  (validate-or-die file)
  (let ((dset (h5dopen2 (id file) dataset-name +h5p-default+)))
    (unless (< 0 dset)
      (return-from open-dataset (create-dataset file dataset-name :type :simple
                                                                  :dims '(3 3))))
    (make-dataset dset file dataset-name "no type" (create-dataspace :scalar))))

(defun write-to-dataset (dataset object)
  (validate-or-die dataset)
  (when (typep (dataspace dataset) 'null-dataspace)
    (error 'dataset-invalid-dataspace :reason "Can't write to Null dataspaces."))
  (unless (or (vectorp object) (atom object))
    (error 'type-error :expected-type '(or vector atom)
                       :datum object))
  (let* ((dspace (dataspace dataset))
         (dtype (data-type dataset))
         (objects (if (vectorp object)
                      object
                      (vector object)))
         (total-space (total-space dspace)))
    (unless (= (length objects) total-space)
      (error 'dataspace-size-mismatch :expected-size total-space
                                      :actual-size (length objects)))
    (%write-to-dataset (id dataset) objects (cffi-type dtype) (id dtype) total-space
                       +h5s-all+ +h5s-all+ +h5p-default+)))

(defun %write-to-dataset (dset-id objects type type-id size mem-space-id
                          file-space-id plist-id)
  (declare (ignore mem-space-id file-space-id plist-id))
  (cffi:with-foreign-object (data type size)    
    (loop :for obj :across objects
          :for i := 0 :then (1+ i)          
          :do (setf (cffi:mem-aref data type i) obj))
    (h5dwrite dset-id type-id  +h5s-all+ +h5s-all+ +h5p-default+ data)))

(defun read-from-dataset (dataset)
  (let* ((dspace (dataspace dataset))
         (dtype (data-type dataset))
         (cffi-type (cffi-type dtype))
         (total-space (total-space dspace)))
    (cffi:with-foreign-object (buf cffi-type total-space)
      (when (< (h5dread (id dataset) (id dtype) +h5s-all+ +h5s-all+ +h5p-default+ buf) 0)
        (error 'dataset-error :reason "Unable to read from dataset"))
      (let ((results (loop :for i :from 0 :upto (1- total-space)
                           :collect
                           (cffi:convert-from-foreign
                            (cffi:mem-aref buf cffi-type i) cffi-type))))
        (etypecase dspace
          (simple-dataspace results)
          (scalar-dataspace (first results)))))))

(define-close ((instance dataset))
  (h5dclose (id instance)))

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
             (hdf5-close ,dset)))))))

(defmacro with-datasets ((&rest datasets) &body body)
  (multiple-value-bind (forms decl) (alexandria:parse-body body)
    `(let (,@(loop :for dataset :in datasets
                   :collect `(,(first dataset) (open-dataset ,@(rest dataset)))))
       ,@decl
       (unwind-protect
            (progn ,@forms)
         (progn
           ,@(loop :for dataset :in datasets
                   :collect `(hdf5-close ,(first dataset))))))))

(defun stupid-dset-test (file)
  (uiop:delete-file-if-exists file)
  (with-hdf5 ((:file f "../test/something.h5" :if-does-not-exist :create)
              (:dataset d f "something"))
    (describe d)
    (write-to-dataset d #(1 2 3 4 5 6 7 8 9))
    (read-from-dataset d)))
