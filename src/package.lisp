(defpackage :cl-hdf5
  (:use #:cl
        #:hdf5)
  (:export
   ;; File functions
   #:is-hdf5-file
   #:create-hdf5-file
   #:open-hdf5-file
   #:close-hdf5-file

   ;; Dataspace functions
   #:create-dataspace
   #:close-dataspace

   ;; Constants
   #:+unlimited+))
