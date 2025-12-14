(defpackage :cl-ky-wgpu/test
  (:use #:cl #:wgpu)
  (:nicknames :wgpu/test)
  (:local-nicknames (:window :glfw/window)
                    (:input :glfw/input))
  (:export :my-pretty-triangle))

(in-package :wgpu/test)

(defun pprint-plist (plist &optional (stream t))
  (format stream "~%~A~20,0T~A" "KEY" "PARAM")
  (format stream "~%~A" (make-string 30 :initial-element #\-))
  (loop for (key value) on plist by #'cddr do
           (format stream "~%~A~20,0T~S" key value))
  (terpri stream))

(defun keyboard-handler (window key scan-code action mods)
  (declare (ignore scan-code mods))
  (format t "HANDLER1[~A]: ~A:~A~%" window key action)
  (when (eq key :glfw-key-escape)
    (window:window-close window))
  t)

(defun test-loop ()
  (glfw:with-glfw (glfw-inst)
    (format t "GLFW HEADER VERSION: ~S~%" (glfw:glfw-version-header glfw-inst))
    (format t "PLATFORM: ~S~%" (glfw:glfw-platform glfw-inst))
    (format t "WAYLAND SUPPORTED?: ~S~%" (glfw:glfw-platform-supported-p glfw-inst :glfw-platform-wayland))
    (window:with-window (window
                         "hello" 600 600
                         '((:glfw-window-hint-client-api :glfw-api-no-api)))
      (format t "WINDOW: ~S~%" window)
      (input:input-init)
      (flet ((keyboard-cb (&rest rest)
               (apply #'keyboard-handler (cons window rest))))
        (let ((keyboard (input:make-keyboard)))
          (input:keyboard-acquire keyboard (window:window-get-ptr window))
          (input:keyboard-push-handler keyboard #'keyboard-cb)
          (with-wgpu (wgpu-instance)
            (format t "WGPU: ~S, ptr: ~S~%" wgpu-instance (window:window-get-ptr window))
            (let* ((surface (wgpu/surface:make-surface wgpu-instance window))
                   (adapter (wgpu/adapter:make-adapter wgpu-instance surface))
                   (device (wgpu/device:make-device adapter
                                                    (wgpu/device:make-default-device-descriptor))))
              (format t "ADAPTER: ~S~%" adapter)
              (pprint-plist (wgpu/adapter:adapter-get-info adapter))

              (format t "DEVICE: ~S~%" device)

              (do () ((window:window-close-p window))
                (glfw/%instance:wait-events))

              (wgpu/device:device-release device)
              (wgpu/adapter:adapter-release adapter)
              (wgpu/surface:surface-release surface)))

          (input:keyboard-release keyboard))
        (input:input-deinit)
        (format t "DONE~%")))))

(defun my-pretty-triangle ()
  (test-loop))
