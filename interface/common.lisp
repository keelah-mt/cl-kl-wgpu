(defpackage cl-kl-wgpu/interface/common
  (:nicknames :wgpu/common)
  (:use #:cl)
  (:import-from #:wgpu/%misc
                #:def-enum-type)
  (:local-nicknames (#:%f #:wgpu/ffi))
  (:export
   :blend-operation
   :blend-factor
   :compare-function
   :optional-bool
   :texture-format
   :primitive-topology
   :index-format
   :front-face
   :cull-mode))

(in-package :wgpu/common)

(def-enum-type %f:wgpu-blend-operation blend-operation)
(def-enum-type %f:wgpu-blend-factor blend-factor)
(def-enum-type %f:wgpu-compare-function compare-function)
(def-enum-type %f:wgpu-optional-bool optional-bool)
(def-enum-type %f:wgpu-texture-format texture-format)
(def-enum-type %f:wgpu-primitive-topology primitive-topology)
(def-enum-type %f:wgpu-index-format index-format)
(def-enum-type %f:wgpu-front-face front-face)
(def-enum-type %f:wgpu-cull-mode cull-mode)

