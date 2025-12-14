(defpackage :cl-kl-wgpu/interface.errors
  (:use #:cl)
  (:nicknames :wgpu/errors)
  (:export :make-unsupported-platform-error))

(in-package :wgpu/errors)

(define-condition unsupported-platform (simple-error)
  ((platform :initarg :platform
              :reader unsupported-platform-platform
              :type glfw:platform))
  (:report (lambda (c s)
             (format s "Platform ~A is not supported."
                     (unsupported-platform-platform c)))))

(defun make-unsupported-platform-error (platform)
  (make-instance 'unsupported-platform :platform platform))