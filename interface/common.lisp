(defpackage cl-kl-wgpu/interface/common
  (:nicknames :wgpu/common)
  (:use #:cl)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:wgpu/%misc
                #:def-enum-type)
  (:local-nicknames (#:%f #:wgpu/ffi))
  (:export
   :present-mode
   :composite-alpha-mode
   :blend-operation
   :blend-factor
   :compare-function
   :optional-bool
   :texture-format
   :texture-aspect
   :texture-view-dimension
   :primitive-topology
   :index-format
   :front-face
   :cull-mode
   :surface-texture-status
   :parse-wgpu-texture-usage))

(in-package :wgpu/common)

(def-enum-type %f:wgpu-present-mode present-mode)
(def-enum-type %f:wgpu-composite-alpha-mode composite-alpha-mode)
(def-enum-type %f:wgpu-blend-operation blend-operation)
(def-enum-type %f:wgpu-blend-factor blend-factor)
(def-enum-type %f:wgpu-compare-function compare-function)
(def-enum-type %f:wgpu-optional-bool optional-bool)
(def-enum-type %f:wgpu-texture-format texture-format)
(def-enum-type %f:wgpu-texture-aspect texture-aspect)
(def-enum-type %f:wgpu-texture-view-dimension texture-view-dimension)
(def-enum-type %f:wgpu-primitive-topology primitive-topology)
(def-enum-type %f:wgpu-index-format index-format)
(def-enum-type %f:wgpu-front-face front-face)
(def-enum-type %f:wgpu-cull-mode cull-mode)
(def-enum-type %f:wgpu-surface-get-current-texture-status surface-texture-status)

(define-constant +wgpu-texture-usage-map+
    `((,%f:wgpu-texture-usage-copy-src . :copy-src)
      (,%f:wgpu-texture-usage-copy-dst . :copy-dst)
      (,%f:wgpu-texture-usage-texture-binding . :texture-binding)
      (,%f:wgpu-texture-usage-storage-binding . :storage-binding)
      (,%f:wgpu-texture-usage-render-attachment . :render-attachment))
  :test 'equal)

(defun parse-wgpu-texture-usage (bitmask)
  (if (zerop bitmask)
      (list :none)
      (reduce (lambda (acc entry)
                (if (plusp (logand bitmask (car entry)))
                    (append acc (list (cdr entry)))
                    acc))
              +wgpu-texture-usage-map+
              :initial-value nil)))
