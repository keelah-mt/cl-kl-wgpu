(defpackage :cl-kl-wgpu/surface
  (:use #:cl)
  (:nicknames :wgpu/surface)
  (:local-nicknames (:%s :wgpu/%surface)
                    (:%g :wgpu/%grovel))
  (:export :make-surface
           :surface-release
           :surface-ptr))

(in-package :wgpu/surface)

(defclass surface ()
  ((ptr :initarg :instance-ptr
        :initform (cffi:null-pointer)
        :reader surface-ptr
        :type %g:surface)))

(defun make-surface (wgpu window)
  (let* ((display (glfw/window:get-x11-display))
         (x11-window (glfw/window:window-get-x11 window)))
    (%s:with-x11-surface-descriptor (descriptor display x11-window)
      (let ((surface (%s:create wgpu descriptor)))
        (make-instance 'surface :instance-ptr surface)))))

(defgeneric surface-release (surface)
  (:method ((s surface))
    (with-slots (ptr) s
      (%s:release ptr)
      (setf ptr (cffi:null-pointer)))))