(defpackage :cl-hdf5
  (:use #:cl
        #:hdf5)
  (:export
   ;; Generic
   #:hdf5-close

   ;; Macros
   #:with-hdf5
   #:with-open-hdf5-file
   #:with-open-hdf5-files
   #:with-dataspace
   #:with-datasets
   #:with-dataset
   
   ;; Files
   #:is-hdf5-file
   #:create-hdf5-file
   #:open-hdf5-file
   #:close-hdf5-file
   #:make-hdf5-file

   ;; Dataspaces
   #:create-dataspace
   #:close-dataspace

   ;; Datasets
   #:read-from-dataset
   #:write-to-dataset
   #:open-dataset
   #:create-dataset
   #:make-dataset

   ;; Types
   #:make-datatype
   
   ;; Constants
   #:+unlimited+
   #:+default-properties+
   #:+hdf5-types+

   ;; Errors
   #:hdf5-error   
   #:hdf5-invalid-id
   #:hdf5-close-error
   #:hdf5-file-error
   #:hdf5-file-creation-error
   #:hdf5-file-open-error
   #:dataset-creation-error
   #:dataset-invalid-dataspace
   #:dataspace-error
   #:dataspace-size-mismatch
   #:dataspace-creation-error))
