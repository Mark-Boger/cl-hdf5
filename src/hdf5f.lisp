(in-package :cl-hdf5)

(defclass hdf5-file (hdf5)
  ((%filepath :initarg :filepath
              :reader filepath)))

(defmethod describe-object ((file hdf5-file) stream)
  (format stream "~a is an ~:[closed~;open~] HDF5 file"
          (filepath file)
          (is-open file)))

(defun make-hdf5-file (fd filepath)
  (make-instance 'hdf5-file :fd fd :filepath filepath))

;; This isn't technically in the hdf5f part of the API but _I_ think
;; it makes more sense here.
(defun is-hdf5-file (file-name)
  "Check if a file with the given FILE-NAME is an HDF5 file"
  (let ((is-hdf5 (h5fis-hdf5 (namestring (truename file-name)))))
    (cond
      ((> is-hdf5 0) t)
      ((= is-hdf5 0) nil)
      (t (error "File error")))))

(defun create-hdf5-file (file-name &key (if-exists :error)
                                     (file-creation-property-list :default)
                                     (file-access-property-list :default))
  "Create an HDF5 file with FILE-NAME.

IF-EXITS may be either :error or :supersede. If IF-EXISTS is neither of those an error
will be signaled.

Returns an hid_t that represents the created file."
  (check-arg (member if-exists '(:error :supersede)))
  ;; Right now I'm not doing anything with the property lists
  ;; so just error them out if they aren't default
  (let ((full-name (namestring (pathname file-name)))
        (flags (if (eq if-exists :supersede)
                   +h5f-acc-trunc+
                   +h5f-acc-excl+))
        (creation-property-list (maybe-propeties file-creation-property-list))
        (access-property-list (maybe-propeties file-access-property-list)))
    (let ((file (h5fcreate full-name flags creation-property-list access-property-list)))
      (unless (< 0 file)
        (error 'hdf5-file-creation-error))
      (make-hd))))

(defun open-hdf5-file (file-name &key
                                   (if-does-not-exist :error)
                                   (if-exists :error)
                                   (direction :input-output)
                                   (file-creation-property-list :default)
                                   (file-access-property-list :default))
  (check-arg (member if-does-not-exist '(:error :create)))
  ;; The error option doesn't really make sense so it doesn't do anything
  ;; it's just so that we have parity with the create options
  (check-arg (member if-exists '(:error :supersede)))
  (check-arg (member direction '(:input :input-output)))
  (let ((full-name (namestring (pathname file-name)))
        (create-file (eq if-does-not-exist :create))
        (file-exists (uiop:file-exists-p file-name))
        (trun-file (eq if-exists :supersede))
        (flags (if (eq direction :input-output)
                   +h5f-acc-rdwr+
                   +h5f-acc-rdonly+))
        (access-property-list (maybe-propeties file-access-property-list))
        (creation-property-list (maybe-propeties file-creation-property-list)))
    (when (or (and create-file (not file-exists)) trun-file)
      (return-from open-hdf5-file
        (create-hdf5-file file-name
                          :if-exists if-exists
                          :file-creation-property-list creation-property-list
                          :file-access-property-list access-property-list)))
    (unless file-exists
      (error "File ~a does not exist" file-name))
    (unless (is-hdf5-file file-name)
      (error "File ~a is not an HDF5 file" file-name))
    (h5fopen full-name flags access-property-list)))

(wrap-close-function (h5fclose file-id) :h5i-file close-hdf5-file)

(defmacro with-open-hdf5-file ((file-id file-name &rest options) &body body)
  (multiple-value-bind (forms decls) (alexandria:parse-body body)
    `(let ((,file-id (open-hdf5-file ,file-name ,@options)))
       ,@decls
       (unwind-protect
            (progn ,@forms)
         (close-hdf5-file ,file-id)))))

(defmacro with-open-hdf5-files ((&rest files) &body body)
  (multiple-value-bind (forms decls) (alexandria:parse-body body)
    `(let (,@(loop :for file :in files
                   :collect `(,(first file) (open-hdf5-file ,@(rest files)))))
       ,@decls
       (unwind-protect
            (progn ,@forms))
       (progn ,@(loop :for file :in files
                      :collect `(close-hdf5-file ,(first file)))))))
