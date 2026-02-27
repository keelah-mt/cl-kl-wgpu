(defpackage cl-kl-wgpu/interface/command-encoder
  (:nicknames :wgpu/command-encoder)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:wgpu/%string-view
                #:with-string-view)
  (:import-from #:wgpu/texture
                #:texture-view)
  (:import-from #:wgpu/%misc
                #:wgpu-depth-slice-undefined
                #:define-struct-builder)
  (:import-from #:wgpu/color
                #:color
                #:with-color
                #:make-color)
  (:import-from #:wgpu/render-pass-encoder
                #:assign-render-pass-encoder)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%c #:wgpu/common))
  (:export
   :command-encoder
   :assign-command-encoder
   :build-render-pass-color-attachment
   :build-render-pass-descriptor
   :with-command-encoder-descriptor
   :begin-render-pass
   :finish))

(in-package :wgpu/command-encoder)

;; -------------------- RENDER PASS --------------------

(defstruct render-pass-color-attachment
  (view nil :type (or null texture-view))
  (depth-slice wgpu-depth-slice-undefined :type integer)
  (resolve-target nil :type (or null texture-view))
  (load-op :wgpu-load-op-undefined :type %c:load-op)
  (store-op :wgpu-store-op-undefined :type %c:store-op)
  (clear-value (make-color) :type color))

(define-struct-builder build-render-pass-color-attachment 'render-pass-color-attachment)

(defmethod cffi:translate-into-foreign-memory ((value render-pass-color-attachment)
                                               (type %f:wgpu-render-pass-color-attachment-tclass)
                                               ptr)
  (let ((view (render-pass-color-attachment-view value))
        (target (render-pass-color-attachment-resolve-target value))
        (clear-value (render-pass-color-attachment-clear-value value)))
    (cffi:with-foreign-slots ((%f:next-in-chain 
                               %f:view
                               %f:depth-slice
                               %f:resolve-target
                               %f:load-op
                               %f:store-op
                               %f:clear-value)
                              ptr (:struct %f:wgpu-render-pass-color-attachment))
      (with-color c-color clear-value
        (setf %f:next-in-chain (cffi:null-pointer)
              %f:view (if view (%r:handle view) (cffi:null-pointer))
              %f:depth-slice (render-pass-color-attachment-depth-slice value)
              %f:resolve-target (if target (%r:handle target) (cffi:null-pointer))
              %f:load-op (render-pass-color-attachment-load-op value)
              %f:store-op (render-pass-color-attachment-store-op value)
              %f:clear-value c-color)))))

;; TODO: only color attachments so far, add the rest
(defstruct render-pass-descriptor
  (label "" :type string)
  (color-attachments nil :type list)
  (depth-stencil-attachment nil)
  (occlusion-query-set nil)
  (timestamp-writes nil))

(define-struct-builder build-render-pass-descriptor 'render-pass-descriptor)

;; TODO: incomplete
(defmacro with-render-pass-descriptor (var descriptor &body body)
  (with-gensyms (nin lbl cac ca dsa oqc tsw
                     color-count color-vector c-color-att label-view)
    `(let* ((,color-vector
              (coerce (render-pass-descriptor-color-attachments ,descriptor) 'vector))
            (,color-count (length ,color-vector)))
       (uiop:nest
        (cffi:with-foreign-object (,var '(:struct %f:wgpu-render-pass-descriptor)))
        (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                   (,lbl %f:label)
                                   (,cac %f:color-attachment-count)
                                   (,ca %f:color-attachments)
                                   (,dsa %f:depth-stencil-attachment)
                                   (,oqc %f:occlusion-query-set)
                                   (,tsw %f:timestamp-writes))
                                  ,var (:struct %f:wgpu-render-pass-descriptor)))
        (cffi:with-foreign-array
            (,c-color-att
             ,color-vector
             `(:array (:struct %f:wgpu-render-pass-color-attachment) ,,color-count)))
        (with-string-view ,label-view (render-pass-descriptor-label ,descriptor)
          (setf ,nin (cffi:null-pointer)
                ,lbl ,label-view
                ,cac ,color-count
                ,ca ,c-color-att
                ,dsa (cffi:null-pointer)
                ,oqc (cffi:null-pointer)
                ,tsw (cffi:null-pointer))
          ,@body)))))

;; -------------------- COMMAND BUFFER --------------------

(defclass command-buffer (%r:resource) ())

(defun assign-command-buffer (name buffer)
  (declare (type cffi:foreign-pointer buffer))
  (make-instance 'command-buffer :name name :handle buffer))

(defmethod %r:release ((buffer command-buffer))
  (%f:wgpu-command-buffer-release (%r:handle buffer)))

(defmacro with-command-buffer-descriptor (var label &body body)
  (with-gensyms (nin lbl label-view)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-command-buffer-descriptor)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain) (,lbl %f:label))
                                ,var (:struct %f:wgpu-command-buffer-descriptor)))
      (with-string-view ,label-view ,label
        (setf ,nin (cffi:null-pointer)
              ,lbl ,label-view)
        ,@body))))

;; -------------------- COMMAND ENCODER --------------------

(defmacro with-command-encoder-descriptor (var label &body body)
  (with-gensyms (nin lbl lbl-view)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-command-encoder-descriptor)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                 (,lbl %f:label))
                                ,var (:struct %f:wgpu-command-encoder-descriptor)))
      (with-string-view ,lbl-view ,label
        (setf ,nin (cffi:null-pointer)
              ,lbl ,lbl-view)
        ,@body))))

(defclass command-encoder (%r:resource) ())

(defun assign-command-encoder (name encoder)
  (declare (type cffi:foreign-pointer encoder)
           (type string name))
  (make-instance 'command-encoder :name name :handle encoder))

(defmethod %r:release ((encoder command-encoder))
  (%f:wgpu-command-encoder-release (%r:handle encoder)))

(defmethod begin-render-pass ((encoder command-encoder) descriptor)
  (declare (type render-pass-descriptor descriptor))
  (with-render-pass-descriptor c-desc descriptor
    (let ((result (%f:wgpu-command-encoder-begin-render-pass (%r:handle encoder) c-desc)))
      (assign-render-pass-encoder (render-pass-descriptor-label descriptor) result))))

(defmethod finish ((encoder command-encoder) label)
  (with-command-buffer-descriptor c-desc label
    (let ((result (%f:wgpu-command-encoder-finish (%r:handle encoder) c-desc)))
      (assign-command-buffer label result))))
