(defpackage :cl-kl-wgpu/adapter
  (:use #:cl)
  (:nicknames :wgpu/adapter)
  (:local-nicknames (:bb :blackbird)
                    (:s :wgpu/surface)
                    (:%a :wgpu/%adapter)
                    (:%g :wgpu/%grovel))
  (:import-from :cffi
                #:with-foreign-object)
  (:export :adapter
           :adapter-ptr
           :make-adapter
           :adapter-release
           :adapter-get-info))

(in-package :wgpu/adapter)

;; -------------------- ERRORS --------------------

(define-condition adapter-info-error (simple-error)
  ((status :reader aie-status
           :initarg :status
           :type integer))
  (:report (lambda (c s)
             (format s "Cannot get adapter info, ~A" (aie-status c)))))

(defun make-adapter-info-error (status)
  (make-instance 'adapter-info-error :status status))

;; -------------------- ADAPTER  --------------------

(defclass adapter ()
  ((ptr :initarg :adapter
        :initform (cffi:null-pointer)
        :reader adapter-ptr
        :type %g:adapter)))

(defun make-adapter (wgpu surface)
  (let ((adapter-ptr (%a:request-adapter wgpu (s:surface-ptr surface))))
    (make-instance 'adapter :adapter adapter-ptr)))

(defgeneric adapter-release (adapter)
  (:method ((a adapter))
    (with-slots (ptr) a
      (%a:release-adapter ptr)
      (setf ptr (cffi:null-pointer)))))

(defgeneric adapter-get-info (adapter)
  (:method ((a adapter))
    (with-foreign-object (c-info-ptr '(:struct %g:adapter-info))
      (let ((status (%a:adapter-get-info (adapter-ptr a) c-info-ptr)))
        (if (eq status :wgpu-status-success)
            (let ((result (cffi:convert-from-foreign c-info-ptr '(:struct %g:adapter-info))))
              (%a:adapter-get-info-free (cffi:mem-ref c-info-ptr '(:struct %g:adapter-info)))
              result)
            (error (make-adapter-info-error status)))))))