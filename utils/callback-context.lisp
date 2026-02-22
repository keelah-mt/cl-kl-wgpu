(defpackage :cl-kl-wgpu/utils/%cb-context
  (:nicknames :wgpu/%cb-context)
  (:use #:cl)
  (:import-from #:alexandria #:with-gensyms)
  (:local-nicknames (#:bb #:blackbird)
                    (#:%f #:wgpu/ffi))
  (:export
   :request-resource
   :with-callback-info
   :make-device-lost-callback
   :make-uncaptured-error-callback
   :make-request-resource-callback))

(in-package :wgpu/%cb-context)

;; -------------------- ERRORS --------------------

(define-condition request-callback-error (error)
  ((status :reader status
           :initarg :status
           :type integer)
   (resource :reader resource
             :initarg :resource
             :type string)
   (message :reader message
            :initarg :message
            :type string))
  (:report (lambda (c s)
             (format s "Cannot request ~A, ~A, ~A."
                     (resource c)
                     (status c)
                     (message c)))))

(defun make-request-callback-error (resource status message)
  (make-instance 'request-callback-error :resource resource :status status :message message))

(define-condition unknown-handle-error (error)
  ((handle :reader handle
           :initarg :handle
           :type integer))
  (:report (lambda (c s)
             (format s "Attempt to remove unknown callback handle ~D." (handle c)))))

(defun make-unknown-handle-error (handle)
  (make-instance 'unknown-handle-error :handler handle))

(define-condition promise-resolve-error (error)
  ((promise :reader promise
            :initarg :promise
            :type bb:promise)
   (value :reader value
          :initarg :value
          :type t))
  (:report
   (lambda (c s)
     (format s "An error was raised in promise-wait while resolving ~S with value ~S."
             (promise c)
             (value c)))))

(defun make-promise-resolve-error (promise value)
  (make-instance 'promise-resolve-error :promise promise :value value))

;; -------------------- PROMISE CONTEXT --------------------

(defstruct promise-context
  (resolve nil :type function)
  (reject nil :type function))

(defvar *context-handle-table* (make-hash-table))
(defvar *handle-counter* 0)
(defvar *table-lock* (bt2:make-lock))

(defun add-callback-handle (resolver rejecter)
  (declare (type function resolver)
           (type function rejecter))
  (bt2:with-lock-held (*table-lock*)
    (let ((handle (incf *handle-counter*)))
      (setf (gethash handle *context-handle-table*)
            (make-promise-context :resolve resolver :reject rejecter))
      handle)))

(defun remove-callback-handle (handle)
  (let ((was-removed (bt2:with-lock-held (*table-lock*)
                       (remhash handle *context-handle-table*))))
    (unless was-removed
      (error (make-unknown-handle-error handle)))))

(defun get-context-from-handle (handle)
  (bt2:with-lock-held (*table-lock*)
    (gethash handle *context-handle-table*)))

;; -------------------- CALLBACK HELPERS --------------------

;; TODO: calling this form make-request-resource-callback gives some SAP to pointer warnings
(defun handle-request-callback (status resource resource-name handle-ptr status-check)
  (declare (type (function (t) boolean) status-check)
           (type cffi:foreign-pointer handle-ptr))
  (let* ((handle (cffi:pointer-address handle-ptr))
         (context (get-context-from-handle handle)))
    (remove-callback-handle handle)
    (if context
        (if (funcall status-check status)
            (funcall (promise-context-resolve context) resource)
            ;; TODO: read message
            (funcall (promise-context-reject context)
                     (make-request-callback-error resource-name status "TODO")))
        ;; TODO: well, that is sad... could use some global error reporting
        (format t "cannot find context in handle-request-callback~%"))))

(defmacro make-request-resource-callback (name (status
                                                resource
                                                resource-name
                                                status-check))
  (with-gensyms (message-view-1 message-view-2 user-data-1 user-data-2)
    `(cffi:defcallback ,name :void
         (,status
          ,resource
          ;; FIXME: https://github.com/cffi/cffi/pull/351 
          (,message-view-1 :pointer ;;(:struct %g:string-view)
                           )
          (,message-view-2 :size)
          (,user-data-1 :pointer)
          (,user-data-2 :pointer))
       (declare (ignore ,message-view-1 ,message-view-2 ,user-data-2))
       (handle-request-callback ,(first status)
                                ,(first resource)
                                ,resource-name
                                ,user-data-1
                                ,status-check))))

(defmacro stuff (info callback-mode)
  (when callback-mode info))

(defmacro with-callback-info ((info info-type callback handle &key callback-mode) &body body)
  (with-gensyms (info-ptr nin cb ud1 ud2)
    (let ((set-mode (unless (eq :unsupported callback-mode)
                      `(setf (cffi:foreign-slot-value ,info-ptr '(:struct ,info-type) '%f:mode)
                             (or ,callback-mode :wgpu-callback-mode-allow-spontaneous)))))
      `(cffi:with-foreign-object (,info-ptr '(:struct ,info-type))
         ;; TODO: a bit hackish way to figure out if mode is supported
         (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                    (,cb %f:callback)
                                    (,ud1 %f:userdata1)
                                    (,ud2 %f:userdata2))
                                   ,info-ptr (:struct ,info-type))
           (setf ,nin (cffi:null-pointer)
                 ,cb (cffi:callback ,callback)
                 ,ud1 (cffi:make-pointer ,handle)
                 ,ud2 (cffi:null-pointer))
           ,set-mode
           ;; (let ((,info (cffi:mem-ref ,info-ptr '(:struct ,info-type))))
           ;;   ,@body)
           (let ((,info ,info-ptr))
             ,@body)
           )))))

;; -------------------- PROMISE HELPERS --------------------

(defun promise-wait (promise on-resolved on-rejected)
  (let ((settled (bt2:make-condition-variable))
        (settled-lock (bt2:make-lock))
        (settled-value nil))
    (flet ((ensure-resolved (result)
             (unless settled-value
               (setf settled-value (make-promise-resolve-error promise result)))))
      (bb:catcher
       (bb:attach promise
                  (lambda (value)
                    (bt2:with-lock-held (settled-lock)
                      (unwind-protect
                           (setf settled-value (funcall on-resolved value))
                        (progn
                          (ensure-resolved value)
                          (bt2:condition-notify settled))))))
       (t (err)
          (bt2:with-lock-held (settled-lock)
            (unwind-protect
                 (setf settled-value (funcall on-rejected err))
              (progn
                (ensure-resolved err)
                (bt2:condition-notify settled))))))

      (bt2:with-lock-held (settled-lock)
        (do () (settled-value)
          (bt2:condition-wait settled settled-lock)))
      
      settled-value)))

;; -------------------- REQUEST RESOURCE --------------------

(defmacro request-resource ((callback-info callback-info-type callback
                             &key callback-mode extra-info)
                            &body body)
  (let ((cb-handle (gensym)))
    `(funcall
      (promise-wait
       (bb:create-promise
        (lambda (resolver rejecter)
          ;; TODO: would be nice to make sure handle is always removed
          (let ((,cb-handle (add-callback-handle resolver rejecter)))
            (with-callback-info (,callback-info
                                 ,callback-info-type
                                 ,callback
                                 ,cb-handle
                                 :callback-mode
                                 ,callback-mode)
              ,@body))))
       (lambda (resource) (lambda () (values resource ,extra-info)))
       (lambda (err) (lambda () (error err)))))))
