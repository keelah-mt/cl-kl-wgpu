(defpackage cl-kl-wgpu/interface/render-pass-encoder
  (:nicknames :wgpu/render-pass-encoder)
  (:use #:cl)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi))
  (:export
   :render-pass-encoder
   :assign-render-pass-encoder
   :set-pipeline
   :draw
   :end))

(in-package :wgpu/render-pass-encoder)

(defclass render-pass-encoder (%r:resource) ())

(defun assign-render-pass-encoder (name encoder)
  (declare (type cffi:foreign-pointer encoder))
  (make-instance 'render-pass-encoder :name name :handle encoder))

(defmethod %r:release ((encoder render-pass-encoder))
  (%f:wgpu-render-pass-encoder-release (%r:handle encoder)))

(defmethod set-pipeline ((encoder render-pass-encoder) pipeline)
  (%f:wgpu-render-pass-encoder-set-pipeline (%r:handle encoder) (%r:handle pipeline)))

(defmethod draw ((encoder render-pass-encoder)
                 vertex-count instance-count first-vertex first-instance)
  (%f:wgpu-render-pass-encoder-draw (%r:handle encoder)
                                    vertex-count
                                    instance-count
                                    first-vertex
                                    first-instance))

(defmethod end ((encoder render-pass-encoder))
  (%f:wgpu-render-pass-encoder-end (%r:handle encoder)))
