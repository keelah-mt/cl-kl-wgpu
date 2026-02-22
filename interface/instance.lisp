(defpackage :cl-kl-wgpu/interface/instance
  (:nicknames :wgpu/instance)
  (:use #:cl)
  (:local-nicknames (#:%r #:wgpu/resource))
  (:export :create
           :release
           :with-wgpu))

(in-package :wgpu/instance)

;; -------------------- ERRORS --------------------

(define-condition cannot-create-instance (error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Cannot create WGPU instance, null value returned."))))

(defun make-cannot-create-instance ()
  (make-instance 'cannot-create-instance))

;; -------------------- RESOURCE --------------------

(defclass wgpu (%r:resource) ())

(defmethod %r:release ((self wgpu))
  (wgpu/ffi:wgpu-instance-release (%r:handle self)))

(defun create (descriptor)
  ;;TODO: use descriptor
  (declare (ignore descriptor))
  (let ((instance (wgpu/ffi:wgpu-create-instance (cffi:null-pointer))))
    (when (cffi:null-pointer-p instance)
      (error (make-cannot-create-instance)))
    (make-instance 'wgpu :handle instance :name "WGPU")))

(defmacro with-wgpu ((wgpu) &body body)
  (let ((instance (gensym)))
    `(let ((,instance (create "TODO")))
       (unwind-protect
            (let ((,wgpu ,instance))
              ,@body)
         (%r:release ,instance)))))
