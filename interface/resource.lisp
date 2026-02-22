(defpackage cl-kl-wgpu/interface/resource
  (:nicknames :wgpu/resource)
  (:use #:cl)
  (:export :resource
           :handle
           :name
           :release))

(in-package :wgpu/resource)

(define-condition release-error (error)
  ((resource :reader resource
             :initarg :resource
             :type resource))
  (:report (lambda (c s)
             (format s "Cannot release ~S, not acquired." (resource c)))))

(defun make-release-error (resource)
  (make-instance 'release-error :resource resource))

(defclass resource ()
  ((handle :reader handle
           :initarg :handle
           :initform (cffi:null-pointer) 
           :type (cffi:foreign-pointer))
   (name :reader name
         :initarg :name
         :type string)))

(defgeneric release (resource))

(defmethod print-object ((r resource) s)
  (print-unreadable-object (r s :type t)
    (format s "~A ~S" (string-upcase (name r)) (handle r))))

(defmethod release :around ((self resource))
  (when (cffi:null-pointer-p (handle self))
    (error (make-release-error self)))

  (unwind-protect (call-next-method)
    (setf (slot-value self 'handle) (cffi:null-pointer))))
