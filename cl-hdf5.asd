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
               (:file "hdf5f" :depends-on ("utils"))
               (:file "hdf5s" :depends-on ("utils"))
               (:file "hdf5d" :depends-on ("utils"))
               (:file "macros" :depends-on ("hdf5f"))))
