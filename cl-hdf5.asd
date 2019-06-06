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
               
               (:module "properties"
                :depends-on ("hdf5")
                :components ((:file "properties")
                             (:file "constants" :depends-on ("properties"))))
               
               (:module "files"
                :depends-on ("properties")
                :components ((:file "files")))
               
               (:module "dataspaces"
                :depends-on ("properties")
                :components ((:file "dataspaces")))
               
               (:module "datatypes"
                :depends-on ("properties")
                :components ((:file "datatypes")))
               
               (:module "datasets"
                :depends-on ("properties"
                             "dataspaces"
                             "datatypes"
                             "files")
                :components ((:file "datasets")))
               
               (:file "macros" :depends-on ("properties"
                                            "files"
                                            "dataspaces"
                                            "datatypes"
                                            "datasets"))))
