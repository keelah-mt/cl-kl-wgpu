(defpackage cl-kl-wgpu/device
  (:use #:cl)
  (:nicknames :wgpu/device)
  (:local-nicknames (:bb :blackbird)
                    (:a :wgpu/adapter)
                    (:%d :wgpu/%device)
                    (:%g :wgpu/%grovel)
                    (:%c :wgpu/%cb-context)
                    (:%u :wgpu/%utils))
  (:export :make-default-device-descriptor
           :make-device
           :device-release))

(in-package :wgpu/device)

;; -------------------- QUEUE DESCRIPTOR --------------------

(defclass queue-descriptor ()
  ;; TODO: for now a default descriptor is sufficient
  ())

(defgeneric queue-descriptor-convert (descriptor)
  (:method ((self queue-descriptor))
    (cffi:convert-to-foreign self '(:struct %g:queue-descriptor))))

(defun make-default-queue-descriptor ()
  (make-instance 'queue-descriptor))

(defmethod cffi:translate-into-foreign-memory (descriptor
                                               (type %g:queue-descriptor-tclass)
                                               pointer)
  (cffi:with-foreign-slots ((%g:qd-next %g:qd-label)
                            pointer (:struct %g:queue-descriptor))
    (setf %g:qd-next (cffi:null-pointer)
          %g:qd-label (%u:string-view-convert "hello"))))

;; -------------------- DEVICE DESCRIPTOR --------------------

(defclass device-descriptor ()
  ;; TODO: for now a default descriptor is sufficient
  ())

(defgeneric device-descriptor-convert (descriptor)
  (:method ((self device-descriptor))
    (cffi:convert-to-foreign self '(:struct %g:device-descriptor))))

(defun make-default-device-descriptor ()
  (make-instance 'device-descriptor))


(defmethod cffi:translate-into-foreign-memory (descriptor
                                               (type %g:device-descriptor-tclass)
                                               pointer)
  (cffi:with-foreign-slots ((%g:dd-next
                              %g:dd-label
                              %g:dd-required-feature-count
                              %g:dd-required-features
                              %g:dd-required-limits
                              %g:dd-default-queue
                              %g:dd-device-lost-callback-info
                              %g:dd-uncaptured-error-callback-info)
                            pointer (:struct %g:device-descriptor))
    (setf %g:dd-next (cffi:null-pointer)
          %g:dd-label (%u:string-view-convert "default-device-descriptor")
          %g:dd-required-feature-count 0
          %g:dd-required-features (cffi:null-pointer)
          %g:dd-required-limits (cffi:null-pointer)
          %g:dd-default-queue (queue-descriptor-convert (make-default-queue-descriptor))
          %g:dd-device-lost-callback-info (cffi:callback %d:cb-device-lost)
          %g:dd-uncaptured-error-callback-info (cffi:callback %d:cb-uncaptured-error))))

;; -------------------- DEVICE --------------------

(defclass device ()
  ((ptr :reader device-ptr
        :initarg :device-ptr
        :initform (cffi:null-pointer)
        :type %g:device)))

(defun make-device (adapter descriptor)
  (let ((device-ptr (%d:request-device (a:adapter-ptr adapter)
                                       (device-descriptor-convert descriptor))))
    (make-instance 'device :device-ptr device-ptr)))

;; TODO: check for null
(defgeneric device-release (device)
  (:method ((self device))
    (with-slots (ptr) self
      (%d:release-device ptr)
      (setf ptr (cffi:null-pointer)))))