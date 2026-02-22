(defpackage cl-kl-wgpu/interface/pipeline-layout
  (:nicknames :wgpu/pipeline-layout)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%sv #:wgpu/%string-view))
  (:export
   :pipeline-layout
   :make-pipeline-layout-descriptor
   :make-pipeline-layout))

(in-package :wgpu/pipeline-layout)

;; this expands to a lot of configuration, but for now it can be skipped
;; TODO: implement complete bind groups support
(defmacro with-pipeline-layout-descriptor (var label &body body)
  (with-gensyms (label-view nin l bglc bgl)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-pipeline-layout-descriptor)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                 (,l %f:label)
                                 (,bglc %f:bind-group-layout-count)
                                 (,bgl %f:bind-group-layouts))
                                ,var (:struct %f:wgpu-pipeline-layout-descriptor)))
      (%sv:with-string-view ,label-view ,label
        (setf ,nin (cffi:null-pointer)
              ,l ,label-view
              ,bgl (cffi:null-pointer)
              ,bglc 0)
        ,@body))))

;; TODO: implement lisp interface to build layouts
(defclass pipeline-layout-descriptor ()
  ((label :reader label
          :initarg :label
          :type string)))

(defun make-pipeline-layout-descriptor (label)
  (make-instance 'pipeline-layout-descriptor :label label))

(defclass pipeline-layout (%r:resource) ())

(defun make-pipeline-layout (name device descriptor)
  (with-pipeline-layout-descriptor c-desc (label descriptor)
    (let ((layout (%f:wgpu-device-create-pipeline-layout (%r:handle device) c-desc)))
      (make-instance 'pipeline-layout :handle layout :name name))))

(defmethod %r:release ((pl pipeline-layout))
  (%f:wgpu-pipeline-layout-release (%r:handle pl)))
