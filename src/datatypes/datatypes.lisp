(in-package :cl-hdf5)

(defclass datatype (hdf5-sentinel hdf5)
  ((%cffi-foreign-type :initarg :cffi-type
                       :reader cffi-type)
   (%name :initarg :name
          :reader name)
   (%hdf5-type :initform :h5i-datatype
               :reader hdf5-type
               :allocation :class)))

(defun make-datatype (datatype-id cffi-type)
  (make-instance 'datatype :id datatype-id :cffi-type cffi-type))

(defmethod describe-object ((datatype datatype) stream)
  (format stream "~&~a is a datatype with ~a as its cffi-type"
          datatype
          (cffi-type datatype)))

;; TODO: Define opaque datatype

;; According to the User's Guide you're able to have the two endpoints of a data
;; transfer have different data types. For example if you have BE floats and LE
;; floats you should be able to, transparently, transfer data back and forth (maybe).

;; We only define our types for the native data types because according to the
;; User's Guide 'native' types are predefined aliases for the architecture-specific
;; memory layout.
(defvar +hdf5-types+ `(:b8     ,(make-datatype +h5t-native-b8+     :uint8)
                       :b16    ,(make-datatype +h5t-native-b16+    :uin16)
                       :b32    ,(make-datatype +h5t-native-b32+    :uint32)
                       :b64    ,(make-datatype +h5t-native-b64+    :uint64)
                       :char   ,(make-datatype +h5t-native-char+   :char)
                       :float  ,(make-datatype +h5t-native-float+  :float)
                       :double ,(make-datatype +h5t-native-double+ :double)
                       :haddr  ,(make-datatype +h5t-native-haddr+  :pointer)
                       :hbool  ,(make-datatype +h5t-native-hbool+  :boolean)
                       :herr   ,(make-datatype +h5t-native-herr+   :int)
                       :hsize  ,(make-datatype +h5t-native-hsize+  :int)
                       :hssize ,(make-datatype +h5t-native-hssize+ :int)
                       :int    ,(make-datatype +h5t-native-int+    :int)
                       :llong  ,(make-datatype +h5t-native-llong+  :llong)
                       :long   ,(make-datatype +h5t-native-long+   :long)
                       ;; NOTE: :void is used as a place holder, if this type
                       ;; is used it will error out.
                       :opaque ,(make-datatype +h5t-native-opaque+ :void)
                       :schar  ,(make-datatype +h5t-native-schar+  :char)
                       :short  ,(make-datatype +h5t-native-short+  :short)
                       :uchar  ,(make-datatype +h5t-native-uchar+  :uchar)
                       :uint   ,(make-datatype +h5t-native-uint+   :uint)
                       :ulong  ,(make-datatype +h5t-native-ulong+  :ulong)
                       :ullong ,(make-datatype +h5t-native-ullong+ :ullong)
                       :ushort ,(make-datatype +h5t-native-ushort+ :ushort)))
