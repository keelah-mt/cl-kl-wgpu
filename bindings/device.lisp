(defpackage cl-kl-wgpu/bindings.device
  (:use #:cl #:cffi)
  (:nicknames :wgpu/%device)
  (:local-nicknames (:%g :wgpu/%grovel)
                    (:%a :wgpu/%adapter)
                    (:%c :wgpu/%cb-context))
  (:export :request-device
           :release-device
           :cb-device-lost
           :cb-uncaptured-error))

(in-package :wgpu/%device)

(defcfun (request-device-cb "wgpuAdapterRequestDevice") (:struct %g:future)
  (adapter %g:adapter)
  (descriptor :pointer (:struct %g:device-descriptor))
  (callback-info (:struct %g:request-resource-callback-info)))

(defcfun (release-device "wgpuDeviceRelease") :void
  (device %g:device))

(%c:make-device-lost-callback cb-device-lost (device reason mv-1 mv-2 ud-1 ud-2)
  (declare (ignore mv-1 mv-2 ud-1 ud-2))
  (format t "[TODO]: DEVICE LOST: ~S ~A~%" device reason))

(%c:make-uncaptured-error-callback cb-uncaptured-error (device error-type mv-1 mv-2 ud-1 ud-2)
  (declare (ignore mv-1 mv-2 ud-1 ud-2))
  (format t "[TODO]: DEVICE UNCAPTURED ERROR: ~S ~A~%" device error-type))

(%c:make-request-resource-callback cb-device ((status %g:request-device-status)
                                              (device %g:device)
                                              mv-1 mv-2
                                              ud-1 ud-2)
  (declare (ignore mv-1 mv-2 ud-2))
  (%c:handle-request-callback status device "device" ud-1 (lambda (s)
                                                            (eq s :wgpu-rds-success))))

(defun request-device-promise (adapter descriptor)
  (bb:create-promise
   (lambda (resolver rejecter)
     (%c:with-request-resource-callback (info
                                         cb-device
                                         :wgpu-cm-allow-spontaneous
                                         resolver rejecter)
       (request-device-cb adapter descriptor info)))))

(defun request-device (adapter descriptor)
  (let ((result (%c:promise-wait (request-device-promise adapter descriptor)
                                 #'identity #'identity)))
    (when (typep result 'error)
      (error result))
    result))
