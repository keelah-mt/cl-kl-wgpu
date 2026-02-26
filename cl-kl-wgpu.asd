(defsystem "cl-kl-wgpu"
  :description "cl-kl-wgpu: FFI for wgpu-native"
  :version "0.0.1"
  :author "Kira Verhovyh <git@keelah.cc>"
  :license "AGPLv3"
  :source-control (:git "git@github.com:keelah-mt/cl-kl-wgpu.git")
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :depends-on (#:closer-mop #:blackbird #:cl-kl-glfw #:cl-kl-wgpu/ffi)
  :in-order-to ((test-op (test-op :cl-kl-wgpu/test)))
  :serial t
  :components
  ((:module "utils"
    :components
    ((:file "callback-context")
     (:file "chained-struct")
     (:file "string-view")
     (:file "misc")))
   (:module "interface"
    :components
    ((:file "common")
     (:file "resource")
     (:file "instance")
     (:file "adapter")
     (:file "device")
     (:file "texture")
     (:file "surface")
     (:file "queue")
     (:file "shader-module")
     (:file "pipeline-layout")
     (:file "render-pipeline")))
   (:file "cl-kl-wgpu")))

(defsystem "cl-kl-wgpu/ffi"
  :description "Bare wgpu-native mappings created by c2ffi"
  :defsystem-depends-on (#:cl-kl-c2ffi)
  :depends-on (#:cffi #:cffi-libffi)
  :pathname "ffi"
  :serial t
  :components ((:c2ffi-lisp-file "wgpu"
                :package #:cl-kl-wgpu/%bindings
                :foreign-library-name wgpu-native-lib
                :foreign-library-spec ((t (:default "libwgpu_native"))))
               (:file "export-all")))

(defsystem "cl-kl-wgpu/test"
  :description "cl-kl-wgpu: test suite"
  :depends-on (#:cffi #:fiveam #:cl-kl-wgpu #:cl-kl-wgpu/ffi)
  :pathname "test"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :wgpu-test-all :cl-kl-wgpu/test/suite)))
  :serial t
  :components
  ((:file "suite")))
