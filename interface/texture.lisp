(defpackage :cl-kl-wgpu/interface/texture
  (:nicknames :wgpu/texture)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:wgpu/%misc
                #:define-struct-builder)
  (:import-from #:wgpu/%string-view
                #:with-string-view)
  (:local-nicknames (#:%f #:wgpu/ffi)
                    (#:%r #:wgpu/resource)
                    (#:%c #:wgpu/common))
  (:export
   :texture
   :assign-texture
   :create-view))

(in-package :wgpu/texture)

;; -------------------- TEXTURE VIEW --------------------

(defstruct texture-view-descriptor
  (label "" :type string)
  (format :wgpu-texture-format-undefined :type %c:texture-format)
  (dimension :wgpu-texture-view-dimension-undefined :type %c:texture-view-dimension)
  (base-mip-level 0 :type integer)
  (mip-level-count 0 :type integer)
  (base-array-layer 0 :type integer)
  (array-layer-count 0 :type integer)
  (aspect :wgpu-texture-aspect-undefined :type %c:texture-aspect)
  (usage %f:wgpu-texture-usage-none :type integer))

(define-struct-builder build-texture-view-descriptor 'texture-view-descriptor)

(defmacro with-texture-view-descriptor (var descriptor &body body)
  (with-gensyms (label-view nin lbl fmt dim bml mlc bal alc asp usg)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-texture-view-descriptor)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                 (,lbl %f:label)
                                 (,fmt %f:format)
                                 (,dim %f:dimension)
                                 (,bml %f:base-mip-level)
                                 (,mlc %f:mip-level-count)
                                 (,bal %f:base-array-layer)
                                 (,alc %f:array-layer-count)
                                 (,asp %f:aspect)
                                 (,usg %f:usage))
                                ,var (:struct %f:wgpu-texture-view-descriptor)))
      (with-string-view ,label-view (texture-view-descriptor-label ,descriptor)
        (setf ,nin (cffi:null-pointer)
              ,lbl ,label-view
              ,fmt (texture-view-descriptor-format ,descriptor)
              ,dim (texture-view-descriptor-dimension ,descriptor)
              ,bml (texture-view-descriptor-base-mip-level ,descriptor)
              ,mlc (texture-view-descriptor-mip-level-count ,descriptor)
              ,bal (texture-view-descriptor-base-array-layer ,descriptor)
              ,alc (texture-view-descriptor-array-layer-count ,descriptor)
              ,asp (texture-view-descriptor-aspect ,descriptor)
              ,usg (texture-view-descriptor-usage ,descriptor))
        ,@body))))

(defmacro with-maybe-texture-view-descriptor (var descriptor &body body)
  `(if ,descriptor
       (with-texture-view-descriptor ,var ,descriptor ,@body)
       (let ((,var (cffi:null-pointer)))
         ,@body)))

(defclass texture-view (%r:resource) ())

(defmethod %r:release ((value texture-view))
  (%f:wgpu-texture-view-release (%r:handle value)))

;; -------------------- TEXTURE -------------------------

(defclass texture (%r:resource) ())

(defun assign-texture (name texture)
  (declare (type string name)
           (type cffi:foreign-pointer texture))
  (make-instance 'texture :name name :handle texture))

(defmethod %r:release ((value texture))
  (%f:wgpu-texture-release (%r:handle value)))

;; TODO: how does this fail? returns null pointer?
(defmethod create-view ((value texture) descriptor)
  (declare (type (or null texture-view-descriptor) descriptor))
  (flet ((run-create (c-desc-ptr)
           (let ((view (%f:wgpu-texture-create-view (%r:handle value) c-desc-ptr))
                 (name (if descriptor
                           (texture-view-descriptor-label descriptor)
                           (format nil "~A-view" (%r:name value)))))
             (make-instance 'texture-view :name name :handle view))))
    (if descriptor
        (with-texture-view-descriptor c-desc descriptor
          (run-create c-desc))
        (run-create (cffi:null-pointer)))))
