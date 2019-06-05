(in-package :cl-hdf5)

(defmacro with-hdf5 ((&rest specifications) &body body)
  (labels ((gen-open (specs)
             (loop :for spec :in specs
                   :when (not (null (cddr spec)))
                     :collect `(setf ,(second spec) (,(let ((type (first spec)))
                                                        (ecase type 
                                                          (:file 'open-hdf5-file)
                                                          (:dataset 'open-dataset)
                                                          (:dataspace 'create-dataspace)))
                                                     ,@(cddr spec)))))
           (gen-close (specs)
             (loop :for spec :in specs
                   :collect `(when ,(second spec) (hdf5-close ,(second spec))))))
    (multiple-value-bind (forms decls) (alexandria:parse-body body)
      `(let (,@(loop :for spec :in specifications
                     :collect `(,(second spec) nil)))
         ,@decls
         (unwind-protect
              (progn
                ,@(gen-open specifications)
                ,@forms)
           ;; Most things in HDF5 are actually fine with the parent parts being
           ;; closed first but I like to close things in reverse order.
           (progn ,@(nreverse (gen-close specifications))))))))
