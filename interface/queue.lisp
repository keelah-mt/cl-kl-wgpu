(defpackage :cl-kl-wgpu/interface/queue
  (:nicknames :wgpu/queue)
  (:use #:cl)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi))
  (:export
   :queue
   :make-queue))

(in-package :wgpu/queue)

(defclass queue (%r:resource) ())

(defun make-queue (device name)
  (declare (type wgpu/device:device device)
           (type string name))
  (let ((queue (%f:wgpu-device-get-queue (%r:handle device))))
    (make-instance 'queue :handle queue :name name)))

(defmethod %r:release ((q queue))
  (%f:wgpu-queue-release (%r:handle q)))


