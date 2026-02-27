(defpackage :cl-kl-wgpu/interface/%string-view
  (:nicknames :wgpu/%string-view)
  (:use #:cl)
  (:local-nicknames (#:%f #:wgpu/ffi))
  (:import-from #:alexandria
                #:define-constant
                #:with-gensyms)
  (:import-from #:wgpu/%misc
                #:wgpu-strlen)
  (:export
   :with-string-view
   :with-351-workaround
   :get-l-string))

(in-package :wgpu/%string-view)

;; unfortunately we need to track ownership of string-view strings:
;; 1. We create some ourselves, these are easy - we free them
;; 2. Some come from C, but it wants them back, see:
;;    https://webgpu-native.github.io/webgpu-headers/Ownership.html#ReturnedWithOwnership
;; So we need to track 2. and prevent any attempts to reallocate C strings, losing the
;; original pointers C wants to free

(defclass string-view-wrapper ()
  ((str :initarg :l-string :type string)
   (ptr :initarg :c-ptr :type cffi:foreign-pointer)
   (len :initarg :c-len :type integer)))

(defmethod print-object ((sw string-view-wrapper) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (sw stream)
        (format stream "~A" (slot-value sw 'str)))))

(defmethod get-l-string ((wrapper string-view-wrapper))
  (slot-value wrapper 'str))

(defmacro with-string-view (var str &body body)
  (with-gensyms (c-str data length)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-string-view)))
      (cffi:with-foreign-string (,c-str ,str))
      (cffi:with-foreign-slots (((,data %f:data)
                                 (,length  %f:length))
                                ,var (:struct %f:wgpu-string-view))
        (setf ,data ,c-str
              ,length (length ,str))
        ,@body))))

;; NOTE: this beauty here is to workaround https://github.com/cffi/cffi/pull/351
(defmacro with-351-workaround (var view-ptr view-length &body body)
  (with-gensyms (data length)
    `(cffi:with-foreign-object (,var '(:struct %f:wgpu-string-view))
       (cffi:with-foreign-slots (((,data %f:data) (,length %f:length))
                                 ,var (:struct %f:wgpu-string-view))
         (setf ,data ,view-ptr
               ,length ,view-length)
         (locally
             ,@body)))))

(defmethod cffi:translate-from-foreign (view (type %f:wgpu-string-view-tclass))
  (cffi:with-foreign-slots ((%f:data %f:length) view (:struct %f:wgpu-string-view))
    ;; assume we don't translate our own strings back, who would do that? :-D
    ;; maybe some test code... let's hope for the best
    (let ((l-string (cffi:foreign-string-to-lisp %f:data
                                                 :count (when (/= %f:length wgpu-strlen)
                                                          %f:length))))
      (make-instance 'string-view-wrapper :l-string l-string
                                          :c-ptr %f:data
                                          :c-len %f:length))))

(defmethod cffi:translate-into-foreign-memory ((value string-view-wrapper)
                                               (type %f:wgpu-string-view-tclass)
                                               view-ptr)
  "This method handles translations of strings we received from the lib.
The point is to make sure we return the same pointer the lib passed to us, so it can free it."
  (with-slots (ptr len) value
    (cffi:with-foreign-slots ((%f:data %f:length) view-ptr (:struct %f:wgpu-string-view))
      (setf %f:data ptr
            %f:length len))))

