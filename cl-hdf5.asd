(asdf:defsystem :cl-hdf5
  :description "Wrapper around hdf5-cffi"
  :author "Mark Boger <93mar.bog@gmail.com>"
  :version "0.0.0"
  :licence "MIT"
  :depends-on (:hdf5-cffi
               :cffi
               :alexandria)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "hdf5" :depends-on ("utils"
                                          "conditions"))
               (:file "hdf5p" :depends-on ("hdf5"))
               (:file "hdf5f" :depends-on ("hdf5"
                                           "hdf5p"))
               (:file "hdf5s" :depends-on ("hdf5"
                                           "hdf5p"))
               (:file "hdf5d" :depends-on ("hdf5"
                                           "hdf5p"))
               (:file "hdf5t" :depends-on ("hdf5"
                                           "hdf5p"))
               (:file "macros" :depends-on ("hdf5f"
                                            "hdf5s"
                                            "hdf5d"
                                            "hdf5t"))))
