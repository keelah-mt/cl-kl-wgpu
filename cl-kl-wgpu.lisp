(defpackage :cl-kl-wgpu
  (:use #:cl)
  (:nicknames :wgpu)
  (:local-nicknames (:window :glfw/window)
                    (:input :glfw/input)
                    (:%f :wgpu/ffi)
                    (:%rp :wgpu/render-pipeline))
  (:export :my-pretty-triangle))

(in-package :wgpu)

(defun keyboard-handler (window key scan-code action mods)
  (declare (ignore scan-code mods))
  (format t "HANDLER1[~A]: ~A:~A~%" window key action)
  (when (eq key :glfw-key-escape)
    (window:window-close window))
  t)

(defun get-x11-surface (wgpu window)
  (let ((x11-display (glfw/window:get-x11-display))
        (x11-window (glfw/window:window-get-x11 window)))
    (wgpu/surface:with-surface-source-xlib (source x11-display x11-window)
      (wgpu/surface:make-surface "x11-surface" wgpu source))))

;; (defun get-wayland-surface (wgpu window)
  ;; (let ((wl-display (glfw/window:get-wayland-display))
        ;; (wl-surface (glfw/window:window-get-wayland window)))
    ;; (wgpu/surface:with-surface-source-wayland (source wl-display wl-surface)
      ;; (wgpu/surface:create wgpu source))))

(defun test-loop ()
  (glfw:with-glfw (glfw-inst)
    (format t "GLFW HEADER VERSION: ~S~%" (glfw:glfw-version-header glfw-inst))
    (format t "PLATFORM: ~S~%" (glfw:glfw-platform glfw-inst))
    (format t "WAYLAND SUPPORTED?: ~S~%" (glfw:glfw-platform-supported-p glfw-inst :glfw-platform-wayland))
    (window:with-window (window
                         "hello" 600 600
                         '((:glfw-window-hint-client-api :glfw-api-no-api)))
      (format t "WINDOW: ~S, ~S~%" window (window:window-get-ptr window))
      (input:input-init)
      (flet ((keyboard-cb (&rest rest)
               (apply #'keyboard-handler (cons window rest))))
        (let ((keyboard (input:make-keyboard)))
          (input:keyboard-acquire keyboard (window:window-get-ptr window))
          (input:keyboard-push-handler keyboard #'keyboard-cb)
          (wgpu/instance:with-wgpu (wgpu)
            (format t "WGPU: ~S, ptr: ~S~%" wgpu (window:window-get-ptr window))
            (let* ((surface (get-x11-surface wgpu window))
                   (adapter (wgpu/adapter:make-adapter "adapter" wgpu surface))
                   (surface-caps (wgpu/surface:get-capabilities surface adapter))
                   (device (wgpu/device:make-device "device" adapter
                                                    (wgpu/device:make-device-descriptor "desc")))
                   (queue (wgpu/queue:make-queue device "queue-1"))
                   (shader-module
                     (wgpu/shader-module:make-shader-module
                      device
                      (wgpu/shader-module:load-source-from-file
                       "shader.wgsl" wgpu/shader-module:shader-language-wgsl)
                      "mah-shader-module"))
                   (pipeline-layout
                     (wgpu/pipeline-layout:make-pipeline-layout
                      "pl"
                      device
                      (wgpu/pipeline-layout:make-pipeline-layout-descriptor "desc")))
                   (rp-desc (%rp:build-render-pipeline-descriptor
                             :label "holy triangle"
                             :layout pipeline-layout
                             :vertex `(:module ,shader-module :entry-point "vs_main")
                             :fragment `(:module ,shader-module
                                         :entry-point "fs_main"
                                         :targets ,(list (%rp:build-color-target-state
                                                          :texture-format
                                                          (first (wgpu/surface:texture-formats surface-caps)))))
                             :primitive '(:topology :wgpu-primitive-topology-triangle-list)
                             :multisample '(:count 1 :mask #xFFFFFFFF)))
                   (render-pipeline (wgpu/render-pipeline:make-render-pipeline "pipeline"
                                                                               device
                                                                               rp-desc))
                   (should-reconf-surface t)
                   (fatal-surface-status nil))
              (format t "SURFACE: ~S~%" surface)
              (format t "SURFACE CAPS: ~S~%" surface-caps)
              (format t "ADAPTER: ~S~%" adapter)
              (format t "~A~%" (wgpu/adapter:get-info adapter))
              (format t "DEVICE: ~S~%" device)
              (format t "QUEUE: ~S~%" queue)
              (format t "SHADER-MODULE: ~S~%" shader-module)
              (format t "PIPELINE-LAYOUT: ~A~%" pipeline-layout)
              (format t "RENDER-PIPELINE: ~S~%" render-pipeline)
              ;;
              (do () ((glfw/window:window-close-p window) fatal-surface-status)
                (glfw/%instance:poll-events)

                (when should-reconf-surface
                  (progn
                    (format t ">>> RECONFIGURE SURFACE~%")
                    (multiple-value-bind (width height) (cl-kl-glfw/window:window-size window)
                      (format t ">>> WINDOW SIZE: ~D, ~D~%" width height)
                      (let ((conf (wgpu/surface:build-surface-configuration
                                   :width width
                                   :height height
                                   :device device
                                   :format (first (wgpu/surface:texture-formats surface-caps))
                                   :usage %f:wgpu-texture-usage-render-attachment
                                   :present-mode :wgpu-present-mode-fifo
                                   :alpha-mode (first (wgpu/surface:alpha-modes surface-caps)))))
                        (wgpu/surface:configure surface conf)
                        (setf should-reconf-surface nil)))))

                (wgpu/surface:with-current-texture texture status surface
                  (format t ">>>> GOT SURFACE TEXTURE: ~S~%" status)
                  (case status
                    ((:wgpu-surface-get-current-texture-status-lost
                      :wgpu-surface-get-current-texture-status-timeout
                      :wgpu-surface-get-current-texture-status-outdated)
                     (setf should-reconf-surface t))
                    ((:wgpu-surface-get-current-texture-status-out-of-memory
                      :wgpu-surface-get-current-texture-status-device-lost)
                     (setf fatal-surface-status status)))

                  (unless (or should-reconf-surface fatal-surface-status)
                    (let ((frame (wgpu/texture:create-view texture nil)))
                      (format t ">>> GOT TEXTURE VIEW: ~S~%" frame)
                      (wgpu/resource:release frame)))))

              ;;
              (format t ">>> LOOP DONE[~S], RUN DEINIT~%" fatal-surface-status)

              (wgpu/resource:release pipeline-layout)
              (wgpu/resource:release shader-module)
              (wgpu/resource:release queue)
              (wgpu/resource:release device)
              (wgpu/resource:release adapter)
              (wgpu/resource:release surface)))
          
          (input:keyboard-release keyboard)))
              (input:input-deinit))
        (format t "DONE~%")))

(defun my-pretty-triangle ()
  (test-loop))
