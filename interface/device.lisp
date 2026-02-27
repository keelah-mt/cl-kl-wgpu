 (defpackage cl-kl-wgpu/interface/device
  (:nicknames :wgpu/device)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:with-gensyms)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%cc #:wgpu/%cb-context)
                    (#:%sv #:wgpu/%string-view)
                    (#:%ce #:wgpu/command-encoder))
  (:export
   :make-device-descriptor
   :device
   :make-device
   :create-command-encoder))

(in-package :wgpu/device)

;; -------------------- CALBACK HELPERS ----------

(defmacro make-device-lost-callback (name (device
                                           reason
                                           message-view
                                           user-data-1
                                           user-data-2)
                                     &body body)
  (with-gensyms (message-view-1 message-view-2)
    `(cffi:defcallback ,name :void
         ((,device %f:wgpu-device)
          (,reason %f:wgpu-device-lost-reason)
          (,message-view-1 :pointer)
          (,message-view-2 :size)
          (,user-data-1 :pointer)
          (,user-data-2 :pointer))
       (%sv:with-351-workaround ,message-view ,message-view-1 ,message-view-2
         ,@body))))

(defmacro make-uncaptured-error-callback (name (device
                                                error-type
                                                message-view
                                                user-data-1
                                                user-data-2)
                                          &body body)
  (with-gensyms (message-view-1 message-view-2)
    `(cffi:defcallback ,name :void
         ((,device %f:wgpu-device)
          (,error-type %f:wgpu-error-type)
          (,message-view-1 :pointer)
          (,message-view-2 :size)
          (,user-data-1 :pointer)
          (,user-data-2 :pointer))
       (%sv:with-351-workaround ,message-view ,message-view-1 ,message-view-2
         ,@body))))

(defvar *uncaptured-cbs* (make-hash-table))
(defvar *uncaptured-cbs-counter* 0)
(defvar *lost-cbs* (make-hash-table))
(defvar *lost-cbs-counter* 0)

(deftype cb-type () '(member uncaptured lost))

(defun get-cb-id (cbt)
  (declare (type cb-type cbt))
  (ecase cbt
    (uncaptured
     (incf *uncaptured-cbs-counter*)
     *uncaptured-cbs-counter*)
    (lost
     (incf *lost-cbs-counter*)
     *lost-cbs-counter*)))

(defun get-cb-hash (cbt)
  (declare (type cb-type cbt))
  (ecase cbt
    (uncaptured *uncaptured-cbs*)
    (lost *lost-cbs*)))

(defun get-cb-handler (cb-id cbt)
  (declare (type integer cb-id))
  (gethash cb-id (get-cb-hash cbt)))

(defun set-cb-handler (cb-id cbt handler)
  (declare (type integer cb-id)
           (function handler))
  (setf (gethash cb-id (get-cb-hash cbt))
        handler))

(defun remove-cb-handler (cb-id cbt)
  (declare (type integer cb-id))
  (remhash cb-id (get-cb-hash cbt)))

;; -------------------- CALLBACKS -----------------

(%cc:make-request-resource-callback cb-request-device
    ((status %f:wgpu-request-device-status)
     (device %f:wgpu-device)
     "device"
     (curry #'eq :wgpu-request-device-status-success)))

;; TODO: in both of these there is something sad going on with syntax due to workaround macro
;; needs to be revised
(make-device-lost-callback cb-device-lost
    (device reason message-view user-data-1 user-data-2)
  (let ((user-data-2 user-data-2)
        (handler (get-cb-handler (cffi:pointer-address user-data-1) 'lost))
        (message (cffi:convert-from-foreign message-view '(:struct %f:wgpu-string-view))))
    (declare (type (or null function) handler)
             (ignore user-data-2))
    (if handler
        (funcall handler reason message)
        (format t "WARNING: no device-lost handler is installed for ~S.~%" device))))

(make-uncaptured-error-callback cb-device-uncaptured-error
    (device error-type message-view user-data-1 user-data-2)
  (let ((user-data-2 user-data-2)
        (handler (get-cb-handler (cffi:pointer-address user-data-1) 'uncaptured))
        (message (cffi:convert-from-foreign message-view '(:struct %f:wgpu-string-view))))
    (declare (type (or null function) handler)
             (ignore user-data-2))
    (if handler
        (funcall handler error-type message)
        (format t "WARNING: no device-uncaptured-error handler is installed for ~S.~%" device))))

;; -------------------- QUEUE DESCRIPTOR ---------

(defmacro with-queue-descriptor (var label &body body)
  (with-gensyms (label-view nin l)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-queue-descriptor)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain) (,l %f:label))
                                ,var (:struct %f:wgpu-queue-descriptor)))
      (%sv:with-string-view ,label-view ,label
        (setf ,nin (cffi:null-pointer)
              ,l ,label-view)
        ,@body))))

;; -------------------- DEVICE DESCRIPTOR ---------

;; TODO: implement features/limits etc, for now this is just a stub
;; the only purpose is to create a dummy descriptor so that error/lost callback
;; can be passed
(defclass device-descriptor ()
  ((label :reader label
          :initarg :label
          :type string)
   (required-features :reader required-features
                      :initform '()
                      :initarg :required-features
                      :type list)
   (required-limits :reader required-limits
                    :initform '()
                    :initarg :required-limits
                    :type list)))

(defun make-device-descriptor (label)
  (make-instance 'device-descriptor :label label))

;; -------------------- DEVICE --------------------

;; TODO: not complete yet
(defmacro with-device-descriptor (var lost-cb-id uncaptured-cb-id descriptor &body body)
  (with-gensyms (queue-desc
                 label-view
                 ;; features
                 ;;limits
                 ;; feature-count
                 ;; limits-count
                 lost-cb-info
                 uncaptured-cb-info
                 nin l rfc rf rl dq dlci ueci)
    `(with-slots (label required-features required-limits) ,descriptor
       (let ((,uncaptured-cb-id (get-cb-id 'uncaptured))
             (,lost-cb-id (get-cb-id 'lost))
             ;; (,feature-count (length required-features))
             ;; (,limits-count (length required-limits))
             )
         (uiop:nest
          (cffi:with-foreign-object (,var '(:struct %f:wgpu-device-descriptor)))
          (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                     (,l %f:label)
                                     (,rfc %f:required-feature-count)
                                     (,rf %f:required-features)
                                     (,rl %f:required-limits)
                                     (,dq %f:default-queue)
                                     (,dlci %f:device-lost-callback-info)
                                     (,ueci %f:uncaptured-error-callback-info))
                                    ,var (:struct %f:wgpu-device-descriptor)))
          ;; TODO: this probably will allocate an empty array too, can be optimized
          ;; TODO: count
          ;; (cffi:with-foreign-array (,features required-features
          ;;                                     '(:array %f:wgpu-feature-name 0)))
          ;; TODO: limits are not an array? chained?
          ;; (cffi:with-foreign-array (,limits required-limits
          ;;                                   '(:array %f:wgpu-limits limits-count)))
          (%cc:with-callback-info (,lost-cb-info
                                   %f:wgpu-device-lost-callback-info
                                   cb-device-lost
                                   ,lost-cb-id))
          (%cc:with-callback-info (,uncaptured-cb-info
                                   %f:wgpu-uncaptured-error-callback-info
                                   cb-device-uncaptured-error
                                   ,uncaptured-cb-id
                                   :callback-mode :unsupported))
          (%sv:with-string-view ,label-view label)
          (with-queue-descriptor ,queue-desc (format nil "~A:QUEUE" label)
            (setf ,nin (cffi:null-pointer)
                  ,l ,label-view
                  ,rfc 0                  ;;,feature-count
                  ,rf (cffi:null-pointer) ;; ,features
                  ,rl (cffi:null-pointer)
                  ,dq ,queue-desc
                  ,dlci ,lost-cb-info
                  ,ueci ,uncaptured-cb-info
                  )
            ,@body))))))

(defun request-device (adapter descriptor)
  (with-device-descriptor c-desc lost-cb-id uncaptured-cb-id descriptor
    (%cc:request-resource (cb-info %f:wgpu-request-device-callback-info cb-request-device
                           :extra-info (list :lost lost-cb-id :uncaptured uncaptured-cb-id))
      (%f:wgpu-adapter-request-device
       adapter c-desc (cffi:mem-ref cb-info
                                    '(:struct %f:wgpu-request-device-callback-info))))))

(defclass device (%r:resource)
  ((lost-cb-id :reader lost-cb-id
               :initarg :lost-cb-id
               :type integer)
   (uncaptured-cb-id :reader uncaptured-cb-id
                     :initarg :uncaptured-cb-id
                     :type integer)))

(defun default-lost (device reason message)
  (declare (type device device))
  (format t "WARNING: using default device-lost handler.~%")
  (format t "DEVICE LOST: ~S, ~A~% ~A.~%" device reason message))

(defun default-uncaptured (device error-type message)
  (declare (type device device))
  (format t "WARNING: using default device-uncaptured-error handler.~%")
  (format t "UNCAPTURED DEVICE ERROR: ~S ~A~% ~A.~%" device error-type message))

(defun make-device (name adapter descriptor
                    &key on-device-lost on-uncaptured-error)
  (declare (type device-descriptor descriptor)
           (type (or null function) on-device-lost)
           (type (or null function) on-uncaptured-error))
  (multiple-value-bind (device callbacks) (request-device (%r:handle adapter) descriptor)
    (let* ((lost-cb-id (getf callbacks :lost))
           (uncaptured-cb-id (getf callbacks :uncaptured))
           (instance (make-instance 'device
                                    :handle device
                                    :name name
                                    :lost-cb-id lost-cb-id 
                                    :uncaptured-cb-id uncaptured-cb-id)))
      (set-cb-handler lost-cb-id
                      'lost
                      (curry (or on-device-lost #'default-lost) instance))
      (set-cb-handler uncaptured-cb-id
                      'uncaptured
                      (curry (or on-uncaptured-error #'default-uncaptured) instance))
      instance)))

(defmethod %r:release ((d device))
  (with-slots (lost-cb-id uncaptured-cb-id) d
    (remove-cb-handler lost-cb-id 'lost)
    (remove-cb-handler uncaptured-cb-id 'uncaptured))
  (%f:wgpu-device-release (%r:handle d)))

;; TODO: how does this fail?
(defmethod create-command-encoder ((value device) label)
  (declare (type string label))
  (%ce:with-command-encoder-descriptor c-desc label
    (let ((result (%f:wgpu-device-create-command-encoder (%r:handle value) c-desc)))
      (%ce:assign-command-encoder label result))))
