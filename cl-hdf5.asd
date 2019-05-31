(asdf:defsystem :cl-hdf5
  :description "Wrapper around hdf5-cffi"
  :author "Mark Boger <93mar.bog@gmail.com>"
  :version "0.0.0"
  :licence "MIT"
  :depends-on (:hdf5-cffi)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
               (:file "hdf5f" :depends-on ("package"
                                           "utils"))))
