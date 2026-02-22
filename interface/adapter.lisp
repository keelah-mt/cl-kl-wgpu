(defpackage cl-kl-wgpu/interface/adapter
  (:nicknames :wgpu/adapter)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:with-gensyms)
  (:import-from #:wgpu/%misc
                #:with-zero-object
                #:make-status-error)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%cc #:wgpu/%cb-context)
                    (#:%sv #:wgpu/%string-view))
  (:export :make-adapter
           :get-info))

(in-package :wgpu/adapter)

;; -------------------- ADAPTER INFO ----------------

(defclass adapter-info ()
  ((vendor :initarg :vendor :type string)
   (architecture :initarg :architecture :type string)
   (device :initarg :device :type string)
   (description :initarg :description :type string)
   (backend-type :initarg :backend-type :type %f:backend-type)
   (adapter-type :initarg :adapter-type :type %f:adapter-type)
   (vendor-id :initarg :vendor-id :type %f:vendor-id)
   (device-id :initarg :device-id :type %f:device-id)))

(defmethod describe-object ((info adapter-info) stream)
  (format stream "~A~%" (type-of info))
  (format stream "~%~A~20,0T~A" "KEY" "PARAM")
  (format stream "~%~A" (make-string 30 :initial-element #\-))
  (dolist (slot (closer-mop:class-direct-slots (class-of info)))
    (let ((name (closer-mop:slot-definition-name slot)))
      (format stream "~%~A~20,0T~S" name (slot-value info name)))))

(defmethod print-object ((info adapter-info) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (info stream)
        (with-slots (vendor description backend-type) info
          (format stream "~A ~A ~A" vendor description backend-type)))))

(defun make-from-c-info (c-info)
  (cffi:with-foreign-slots ((%f:vendor
                             %f:architecture
                             %f:device
                             %f:description
                             %f:backend-type
                             %f:adapter-type
                             %f:vendor-id
                             %f:device-id)
                            c-info (:struct %f:wgpu-adapter-info))
    (make-instance 'adapter-info :vendor (%sv:get-l-string %f:vendor)
                                 :architecture (%sv:get-l-string %f:architecture)
                                 :device (%sv:get-l-string %f:device)
                                 :description (%sv:get-l-string %f:description)
                                 :backend-type %f:backend-type
                                 :adapter-type %f:adapter-type
                                 :vendor-id %f:vendor-id
                                 :device-id %f:device-id)))

;; -------------------- ADAPTER  --------------------

(%cc:make-request-resource-callback cb-request-adapter
    ((status %f:wgpu-request-adapter-status)
     (adapter %f:wgpu-adapter)
     "adapter"
     (curry #'eq :wgpu-request-adapter-status-success)))

(defmacro with-request-adapter-options ((options
                                         surface
                                         feature-level
                                         power-preference
                                         backend-type
                                         force-fallback-adapter) &body body)
  (with-gensyms (nic fl pp ffa bt cs)
    `(uiop:nest
      (cffi:with-foreign-object (,options '%f:wgpu-request-adapter-options))
      (cffi:with-foreign-slots (((,nic %f:next-in-chain)
                                 (,fl %f:feature-level)
                                 (,pp %f:power-preference)
                                 (,ffa %f:force-fallback-adapter)
                                 (,bt %f:backend-type)
                                 (,cs %f:compatible-surface))
                                ,options %f:wgpu-request-adapter-options)
        (setf ,nic (cffi:null-pointer)
              ,fl ,feature-level
              ,pp ,power-preference
              ,ffa ,force-fallback-adapter
              ,bt ,backend-type
              ,cs ,surface)
        ,@body))))

(defun request-adapter (wgpu surface)
  (%cc:request-resource (cb-info %f:wgpu-request-adapter-callback-info cb-request-adapter)
    ;; TODO: create aliases for these no-preference values
    (with-request-adapter-options (options surface 0 0 0 0)
      (%f:wgpu-instance-request-adapter
       wgpu options (cffi:mem-ref cb-info '(:struct %f:wgpu-request-adapter-callback-info))))))

(defclass adapter (%r:resource) ())

(defun make-adapter (name wgpu surface)
  (let ((adapter (request-adapter (%r:handle wgpu) (%r:handle surface))))
    (make-instance 'adapter :handle adapter :name name)))

(defmethod %r:release ((a adapter))
  (%f:wgpu-adapter-release (%r:handle a)))

(defmethod get-info ((self adapter))
  (with-zero-object (info '(:struct %f:wgpu-adapter-info))
    (let ((status (%f:wgpu-adapter-get-info (%r:handle self) info)))
      (if (eq status :wgpu-status-success)
          (let ((result (make-from-c-info info)))
            (%f:wgpu-adapter-info-free-members
             (cffi:mem-ref info '(:struct %f:wgpu-adapter-info)))
            result)
          (error (make-status-error (%r:name self)
                                    (symbol-name '%f:wgpu-adapter-get-info)
                                    status))))))
