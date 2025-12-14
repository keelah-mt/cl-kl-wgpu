(defpackage :cl-kl-wgpu/%bindings.callback-context
  (:use #:cl)
  (:nicknames :wgpu/%cb-context)
  (:local-nicknames (:bb :blackbird)
                    (:%g :wgpu/%grovel))
  (:export :allocate-callback-handle
           :handle-request-callback
           :make-device-lost-callback
           :make-uncaptured-error-callback
           :make-request-resource-callback
           :with-request-resource-callback
           :promise-wait))

(in-package :wgpu/%cb-context)

;; -------------------- ERRORS --------------------

(define-condition request-callback-error (simple-error)
  ((status :reader rce-status
           :initarg :status
           :type integer)
   (resource :reader rce-resource
             :initarg :resource
             :type string)
   (message :reader rce-message
            :initarg :message
            :type string))
  (:report (lambda (c s)
             (format s "Cannot request ~A, ~A, ~A."
                     (rce-resource c)
                     (rce-status c)
                     (rce-message c)))))

(defun make-request-callback-error (resource status message)
  (make-instance 'request-callback-error :resource resource :status status :message message))


;; -------------------- PROMISE CONTEXT --------------------

(defstruct promise-context
  (resolve nil :type function)
  (reject nil :type function))

(defvar *context-handle-table* (make-hash-table))
(defvar *handle-counter* 0)
(defvar *table-lock* (bt2:make-lock))

(defun allocate-callback-handle (resolver rejecter)
  (bt2:with-lock-held (*table-lock*)
    (let ((handle (incf *handle-counter*)))
      (setf (gethash handle *context-handle-table*)
            (make-promise-context :resolve resolver :reject rejecter))
      handle)))

(defun get-context-from-handle (handle)
  (bt2:with-lock-held (*table-lock*)
    (gethash handle *context-handle-table*)))

(defun free-context-handle (handle)
  (bt2:with-lock-held (*table-lock*)
    (remhash handle *context-handle-table*)))

;; -------------------- CALLBACK HELPERS --------------------

(defmacro make-device-lost-callback (name (device
                                           reason
                                           message-view-1
                                           message-view-2
                                           user-data-1
                                           user-data-2)
                                     &body body)
  `(cffi:defcallback ,name :void
       ((,device %g:device)
        (,reason %g:device-lost-reason)
        (,message-view-1 :pointer)
        (,message-view-2 :size)
        (,user-data-1 :pointer)
        (,user-data-2 :pointer))
     ,@body))

(defmacro make-uncaptured-error-callback (name (device
                                                error-type
                                                message-view-1
                                                message-view-2
                                                user-data-1
                                                user-data-2)
                                          &body body)
  `(cffi:defcallback ,name :void
       ((,device %g:device)
        (,error-type %g:error-type)
        (,message-view-1 :pointer)
        (,message-view-2 :pointer)
        (,user-data-1 :pointer)
        (,user-data-2 :pointer))
     ,@body))

(defmacro make-request-resource-callback (name (status
                                                resource
                                                message-view-1
                                                message-view-2
                                                user-data-1
                                                user-data-2)
                                          &body body)
  `(cffi:defcallback ,name :void
       (,status
        ,resource
        ;; FIXME: https://github.com/cffi/cffi/pull/351 
        (,message-view-1 :pointer  ;;(:struct %g:string-view)
                         )
        (,message-view-2 :size)
        (,user-data-1 :pointer)
        (,user-data-2 :pointer))
     ,@body))

(defmacro with-request-resource-callback ((info callback callback-mode resolver rejecter)
                                          &body body)
  (let ((handle (gensym))
        (info-ptr (gensym)))
    `(let ((,handle (allocate-callback-handle ,resolver ,rejecter)))
       (cffi:with-foreign-object (,info-ptr '(:struct %g:request-resource-callback-info))
         (cffi:with-foreign-slots ((%g:rrci-next
                                    %g:rrci-mode
                                    %g:rrci-callback
                                    %g:rrci-user-data-1
                                    %g:rrci-user-data-2)
                                   ,info-ptr (:struct %g:request-resource-callback-info))
           (setf %g:rrci-next (cffi:null-pointer)
                 %g:rrci-mode ,callback-mode
                 %g:rrci-callback (cffi:callback ,callback)
                 %g:rrci-user-data-1 (cffi:make-pointer ,handle)
                 %g:rrci-user-data-2 (cffi:null-pointer))
           (let ((,info (cffi:mem-ref ,info-ptr '(:struct %g:request-resource-callback-info))))
             (progn ,@body)))))))

(defun handle-request-callback (status resource resource-name handle-ptr status-check)
  (let* ((handle (cffi:pointer-address handle-ptr))
         (context (get-context-from-handle handle)))
    (free-context-handle handle)
    (if context
        (if (funcall status-check status)
            (funcall (promise-context-resolve context) resource)
            ;; TODO: read message
            (funcall (promise-context-reject context)
                     (make-request-callback-error resource-name status "TODO")))
        ;; TODO: well, that is sad... could use some global error reporting
        (format t "cannot find context in handle-request-callback~%"))))

;; -------------------- PROMISE HELPERS --------------------

(defun promise-wait (promise on-resolved on-rejected)
  (let ((settled (bt2:make-condition-variable))
        (settled-lock (bt2:make-lock))
        (settled-value nil))
    (bb:catcher
     (bb:attach promise
                (lambda (value)
                  (bt2:with-lock-held (settled-lock)
                    (unwind-protect
                         (setf settled-value (funcall on-resolved value))
                      (bt2:condition-notify settled)))))
     (t (err)
        (bt2:with-lock-held (settled-lock)
          (unwind-protect
               (setf settled-value (funcall on-rejected err))
            (bt2:condition-notify settled)))))

    (bt2:with-lock-held (settled-lock)
      (do () (settled-value)
        (bt2:condition-wait settled settled-lock)))
       
    settled-value))
