(defsystem "cl-kl-wgpu"
  :description "cl-kl-wgpu: FFI for wgpu-native"
  :version "0.0.1"
  :author "Kira Verhovyh <git@keelah.cc>"
  :license "BSD 3 Clause"
  :source-control (:git "git@github.com:keelah-mt/cl-kl-wgpu.git")
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:cffi-libffi #:blackbird #:cl-kl-glfw)
  :components ((:module "bindings"
                :serial t
                :components ((:file "grovel-package")
                             (:cffi-grovel-file "grovel-spec")
                             (:file "grovel-export")
                             (:file "callback-context")
                             (:file "utils")
                             (:file "instance")
                             (:file "adapter")
                             (:file "device")
                             (:file "surface")))
               (:module "interface"
                :depends-on ("bindings")
                :components ((:file "errors")
                             (:file "surface")
                             (:file "adapter")
                             (:file "device")
                             (:file "interface")))
               (:file "cl-kl-wgpu"
                :depends-on ("interface"))))
