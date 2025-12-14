(defpackage :cl-kl-wgpu/bindings.surface
  (:use #:cl #:cffi)
  (:nicknames :wgpu/%surface)
  (:local-nicknames (:%i :wgpu/%instance)
                    (:%g :wgpu/%grovel)
                    (:%u :wgpu/%utils))
  (:export :with-x11-surface-descriptor
           :create
           :release))

(in-package :wgpu/%surface)

(defcfun (create "wgpuInstanceCreateSurface") %g:surface
  (instance-ptr %i:instance-ptr)
  (surface-descriptor (:pointer (:struct %g:surface-descriptor))))

(defcfun (release "wgpuSurfaceRelease") :void
  (surface %g:surface))

(defmacro with-surface-descriptor ((descriptor surface label) &body body)
  `(with-foreign-object (,descriptor '(:struct %g:surface-descriptor))
     (with-foreign-slots ((%g:sd-next %g:sd-label)
                          ,descriptor (:struct %g:surface-descriptor))
       (setf %g:sd-label (convert-to-foreign ,label '(:struct %g:string-view))
             %g:sd-next ,surface)
       (progn ,@body))))

(defmacro with-x11-surface-descriptor ((descriptor display window &key (label nil)) &body body)
  (let ((surface (gensym))
        (chain (gensym)))
    `(with-foreign-object (,surface '(:struct %g:surface-source-x11))
       (with-foreign-slots ((%g:ss-x11-chain
                             %g:ss-x11-display
                             %g:ss-x11-window)
                            ,surface (:struct %g:surface-source-x11))
         (let ((,chain (%u:make-chained-struct :type :wgpu-st-surface-source-xlib-window)))
           (setf %g:ss-x11-display ,display
                 %g:ss-x11-window ,window
                 %g:ss-x11-chain (convert-to-foreign ,chain '(:struct %g:chained-struct)))
           (with-surface-descriptor (,descriptor ,surface ,label)
             (progn ,@body)))))))
