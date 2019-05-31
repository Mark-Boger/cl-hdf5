(in-package :cl-hdf5)

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
  (check-arg (eq file-creation-property-list :default))
  (check-arg (eq file-access-property-list :default))
  (let ((full-name (namestring (pathname file-name)))
        (flags (if (eq if-exists :supersede)
                   +h5f-acc-trunc+
                   +h5f-acc-excl+))
        (creation-property-list +h5p-default+)
        (access-property-list +h5p-default+))
    (h5fcreate full-name flags creation-property-list access-property-list)))

(defun open-hdf5-file (file-name &key (if-does-not-exist :error)
                                   (direction :input-output)
                                   (file-creation-property-list :default)
                                   (file-access-property-list :default))
  (check-arg (member if-does-not-exist '(:error :create)))
  (check-arg (member direction '(:input :input-output)))
  (check-arg (eq file-creation-property-list :default))
  (check-arg (eq file-access-property-list :default))
  (let ((full-name (namestring (pathname file-name)))
        (create-file (eq if-does-not-exist :create))
        (file-exists (uiop:file-exists-p file-name))
        (flags (if (eq direction :input-output)
                   +h5f-acc-rdwr+
                   +h5f-acc-rdonly+))
        (access-property-list +h5p-default+))
    (when (and create-file (not file-exists))
      (return-from open-hdf5-file
        (create-hdf5-file file-name
                          :file-creation-property-list file-creation-property-list
                          :file-access-property-list file-access-property-list)))
    (unless file-exists
      (error "File ~a does not exist" file-name))
    (unless (is-hdf5-file file-name)
      (error "File ~a is not an HDF5 file" file-name))
    (h5fopen full-name flags access-property-list)))

(wrap-close-function (h5fclose file-id) :h5i-file close-hdf5-file)

