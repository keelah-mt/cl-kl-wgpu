(defpackage :cl-kl-wgpu/bindings.instance
  (:use #:cl)
  (:nicknames :wgpu/%instance)
  (:local-nicknames (:%g :wgpu/%grovel))
  (:import-from :cffi
                #:defcfun
                #:defctype)
  (:export :instance-descriptor-ptr
           :instance-ptr
           :create
           :release))

(in-package :wgpu/%instance)

(cffi:define-foreign-library libwgpu
  (t (:default "libwgpu_native")))

(cffi:use-foreign-library libwgpu)

;; TODO: instance descriptor unpacks to a lot of typedefs that are not needed atm
;; implement if needed, for now an opaque pointer for references is enough
(defctype instance-descriptor-ptr :pointer)
(defctype instance-ptr :pointer)

(defcfun (create "wgpuCreateInstance") instance-ptr
  (descriptor instance-descriptor-ptr))

(defcfun (release "wgpuInstanceRelease") :void
  (instance instance-ptr))

