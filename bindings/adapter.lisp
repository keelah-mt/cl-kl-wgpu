(defpackage cl-kl-wgpu/bindings.adapter
  (:use #:cl #:cffi)
  (:nicknames :wgpu/%adapter)
  (:local-nicknames (:bb :blackbird)
                    (:%g :wgpu/%grovel)
                    (:%i :wgpu/%instance)
                    (:%u :wgpu/%utils)
                    (:%c :wgpu/%cb-context))
  (:export :request-adapter
           :release-adapter
           :adapter-get-info
           :adapter-get-info-free))

(in-package :wgpu/%adapter)

(defcfun (request-adapter-cb "wgpuInstanceRequestAdapter") (:struct %g:future)
  (instance %i:instance-ptr)
  (options :pointer (:struct %g:request-adapter-options))
  (callback-info (:struct %g:request-resource-callback-info)))

(defcfun (release-adapter "wgpuAdapterRelease") :void
  (adapter %g:adapter))

(defcfun (adapter-get-info "wgpuAdapterGetInfo") %g:status
  (adapter %g:adapter)
  (info :pointer (:struct %g:adapter-info)))

(defcfun (adapter-get-info-free "wgpuAdapterInfoFreeMembers") :void
  "TODO: it is not clear what it tries to free.
Since string views are getting translated from/to memory, pointers to the original strings might be lost and leaking?"
  (info (:struct %g:adapter-info)))

(defmacro with-request-adapter-options ((options
                                         surface
                                         feature-level
                                         power-preference
                                         backend-type
                                         force-fallback-adapter) &body body)
  `(with-foreign-object (,options '(:struct %g:request-adapter-options))
     (with-foreign-slots ((%g:rao-next
                           %g:rao-feature-level
                           %g:rao-power-preference
                           %g:rao-force-fallback-adapter
                           %g:rao-backend-type
                           %g:rao-compatible-surface)
                          ,options (:struct %g:request-adapter-options))
       (setf %g:rao-next (null-pointer)
             %g:rao-feature-level ,feature-level
             %g:rao-power-preference ,power-preference
             %g:rao-force-fallback-adapter ,force-fallback-adapter
             %g:rao-backend-type ,backend-type
             %g:rao-compatible-surface ,surface)
       (progn ,@body))))

(%c:make-request-resource-callback cb-adapter ((status %g:request-adapter-status)
                                               (adapter %g:adapter)
                                               message-view-1 message-view-2
                                               user-data-1 user-data-2)
  (declare (ignore message-view-1 message-view-2 user-data-2))
  (%c:handle-request-callback status adapter "adapter" user-data-1 (lambda (s)
                                                                     (eq s :wgpu-ras-success))))

(defun request-adapter-promise (wgpu surface)
  (bb:create-promise
   (lambda (resolver rejecter)
     ;; TODO: pass as a param instead
     (with-request-adapter-options (options
                                    surface
                                    :wgpu-fl-undefined
                                    :wgpu-pp-undefined
                                    :wgpu-bt-undefined
                                    nil)
       (%c:with-request-resource-callback (info
                                           cb-adapter
                                           :wgpu-cm-allow-spontaneous
                                           resolver rejecter)
         (request-adapter-cb wgpu options info))))))

(defun request-adapter (wgpu surface)
  (let ((result (%c:promise-wait (request-adapter-promise wgpu surface) #'identity #'identity)))
    (when (typep result 'error)
        (error result))
    result))
