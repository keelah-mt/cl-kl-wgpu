(defpackage :cl-kl-wgpu/test/suite
  (:nicknames :wgpu/test/suite)
  (:use #:cl #:fiveam)
  (:local-nicknames (:%f :wgpu/ffi)))

(in-package :wgpu/test/suite)

(def-suite wgpu-test-all
  :description "Test cl-kl-wgpu system")

