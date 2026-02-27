(defpackage :cl-kl-wgpu/interface/queue
  (:nicknames :wgpu/queue)
  (:use #:cl)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi))
  (:export
   :queue
   :make-queue
   :submit))

(in-package :wgpu/queue)

(defclass queue (%r:resource) ())

(defun make-queue (device name)
  (declare (type wgpu/device:device device)
           (type string name))
  (let ((queue (%f:wgpu-device-get-queue (%r:handle device))))
    (make-instance 'queue :handle queue :name name)))

(defmethod %r:release ((queue queue))
  (%f:wgpu-queue-release (%r:handle queue)))

(defmethod submit ((queue queue) commands)
  (declare (type list commands))
  (let* ((cmd-vector (coerce (mapcar #'%r:handle commands) 'vector))
         (cmd-len (length cmd-vector)))
    (cffi:with-foreign-array (c-cmd cmd-vector `(:array %f:wgpu-buffer ,cmd-len))
      (%f:wgpu-queue-submit (%r:handle queue) cmd-len c-cmd))))


