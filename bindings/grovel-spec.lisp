(in-package :wgpu/%grovel.exports)

;; TODO: should be conditional along with platfrom specific types
(include "X11/X.h")

(include "libwgpu-native/webgpu.h")
(include "libwgpu-native/wgpu.h")

;; used to provide 'undefined' i.e. 0 option in various structs
(define "CL_WGPU_UNDEFINED" 0)

(constant (sv-null-cstring "WGPU_STRLEN"))
(ctype x11-window "Window")
(ctype surface "WGPUSurface")
(ctype adapter "WGPUAdapter")
(ctype device "WGPUDevice")
(ctype uint32t "uint32_t")
(ctype uint64t "uint64_t")

(cenum (status)
       ((:wgpu-status-success "WGPUStatus_Success"))
       ((:wgpu-status-error "WGPUStatus_Error")))

(cenum (s-type)
       ((:wgpu-st-shader-source-spirv "WGPUSType_ShaderSourceSPIRV"))
       ((:wgpu-st-shader-source-wgsl "WGPUSType_ShaderSourceWGSL"))
       ((:wgpu-st-shader-source-glsl "WGPUSType_ShaderSourceGLSL"))
       ((:wgpu-st-surface-source-metal-layer "WGPUSType_SurfaceSourceMetalLayer"))
       ((:wgpu-st-surface-source-window-hwnd "WGPUSType_SurfaceSourceWindowsHWND"))
       ((:wgpu-st-surface-source-xlib-window "WGPUSType_SurfaceSourceXlibWindow"))
       ((:wgpu-st-surface-source-wayland-surface "WGPUSType_SurfaceSourceWaylandSurface"))
       ((:wgpu-st-surface-source-android-window "WGPUSType_SurfaceSourceAndroidNativeWindow"))
       ((:wgpu-st-surface-source-xcb-window "WGPUSType_SurfaceSourceXCBWindow")))

(cstruct future "WGPUFuture"
         (future-id "id" :type :uint64))

(cstruct string-view "WGPUStringView"
         (sv-data "data" :type :pointer)
         (sv-data-length "length" :type :size))

(cstruct chained-struct "WGPUChainedStruct"
         (cs-next "next" :type (:pointer (:struct chained-struct)))
         (cs-type "sType" :type s-type))

(cstruct surface-source-x11 "WGPUSurfaceSourceXlibWindow"
         (ss-x11-chain "chain" :type (:struct chained-struct))
         (ss-x11-display "display" :type :pointer)
         (ss-x11-window "window" :type x11-window))

(cstruct surface-descriptor "WGPUSurfaceDescriptor"
         (sd-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (sd-label "label" :type (:struct string-view)))

(cenum (feature-level)
       ((:wgpu-fl-undefined "CL_WGPU_UNDEFINED"))
       ((:wgpu-fl-compatibility "WGPUFeatureLevel_Compatibility"))
       ((:wgpu-fl-core "WGPUFeatureLevel_Core")))

(cenum (power-preference)
       ((:wgpu-pp-undefined "CL_WGPU_UNDEFINED"))
       ((:wgpu-pp-low-power "WGPUPowerPreference_LowPower"))
       ((:wgpu-pp-high-performace "WGPUPowerPreference_HighPerformance")))

(cenum (backend-type)
       ((:wgpu-bt-undefined "CL_WGPU_UNDEFINED"))
       ((:wgpu-bt-null "WGPUBackendType_Null"))
       ((:wgpu-bt-webgpu "WGPUBackendType_WebGPU"))
       ((:wgpu-bt-d3d11 "WGPUBackendType_D3D11"))
       ((:wgpu-bt-d3d12 "WGPUBackendType_D3D12"))
       ((:wgpu-bt-metal "WGPUBackendType_Metal"))
       ((:wgpu-bt-vulkan "WGPUBackendType_Vulkan"))
       ((:wgpu-bt-opengl "WGPUBackendType_OpenGL"))
       ((:wgpu-bt-opengl-es "WGPUBackendType_OpenGLES")))

(cenum (adapter-type)
       ((:wgpu-at-discrete-gpu "WGPUAdapterType_DiscreteGPU"))
       ((:wgpu-at-integrated-gpu "WGPUAdapterType_IntegratedGPU"))
       ((:wgpu-at-cpu "WGPUAdapterType_CPU"))
       ((:wgpu-at-unknown "WGPUAdapterType_Unknown")))

(cenum (error-type)
       ((:wgpu-et-no-error "WGPUErrorType_NoError"))
       ((:wgpu-et-validation "WGPUErrorType_Validation"))
       ((:wgpu-et-out-of-memory "WGPUErrorType_OutOfMemory"))
       ((:wgpu-et-internal "WGPUErrorType_Internal"))
       ((:wgpu-et-unknown "WGPUErrorType_Unknown")))

(cenum (callback-mode)
       ((:wgpu-cm-wait-any-only "WGPUCallbackMode_WaitAnyOnly"))
       ((:wgpu-cm-allow-process-events "WGPUCallbackMode_AllowProcessEvents"))
       ((:wgpu-cm-allow-spontaneous "WGPUCallbackMode_AllowSpontaneous")))

(cenum (request-adapter-status)
       ((:wgpu-ras-success "WGPURequestAdapterStatus_Success"))
       ((:wgpu-ras-instance-dropped "WGPURequestAdapterStatus_InstanceDropped"))
       ((:wgpu-ras-unavailable "WGPURequestAdapterStatus_Unavailable"))
       ((:wgpu-ras-error "WGPURequestAdapterStatus_Error"))
       ((:wgpu-ras-unknown "WGPURequestAdapterStatus_Unknown")))

(cenum (request-device-status)
       ((:wgpu-rds-success "WGPURequestDeviceStatus_Success"))
       ((:wgpu-rds-instance-dropped "WGPURequestDeviceStatus_InstanceDropped"))
       ((:wgpu-rds-error "WGPURequestDeviceStatus_Error"))
       ((:wgpu-rds-unknown "WGPURequestDeviceStatus_Unknown")))

(cenum (device-lost-reason)
       ((:wgpu-dlr-unknown "WGPUDeviceLostReason_Unknown"))
       ((:wgpu-dlr-destroyed "WGPUDeviceLostReason_Destroyed"))
       ((:wgpu-dlr-instance-dropped "WGPUDeviceLostReason_InstanceDropped"))
       ((:wgpu-dlr-failed-creation "WGPUDeviceLostReason_FailedCreation")))

(cstruct request-adapter-options "WGPURequestAdapterOptions"
         (rao-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (rao-feature-level "featureLevel" :type feature-level)
         (rao-power-preference "powerPreference" :type power-preference)
         (rao-force-fallback-adapter "forceFallbackAdapter" :type :boolean)
         (rao-backend-type "backendType" :type backend-type)
         (rao-compatible-surface "compatibleSurface" :type surface))

;; NOTE adapter/device has a specific callback typedef, but we don't differentiate,
;; both are just a :pointer, so we can coerce them both to a generic info structure
(cstruct request-resource-callback-info "WGPURequestAdapterCallbackInfo"
         (rrci-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (rrci-mode "mode" :type callback-mode)
         (rrci-callback "callback" :type :pointer)
         (rrci-user-data-1 "userdata1" :type :pointer)
         (rrci-user-data-2 "userdata2" :type :pointer))

;; TODO: actually not different from resource-callback
(cstruct device-lost-callback-info "WGPUDeviceLostCallbackInfo"
         (rrci-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (rrci-mode "mode" :type callback-mode)
         (rrci-callback "callback" :type :pointer)
         (rrci-user-data-1 "userdata1" :type :pointer)
         (rrci-user-data-2 "userdata2" :type :pointer))

(cstruct uncaptured-error-callback-info "WGPUUncapturedErrorCallbackInfo"
         (dlci-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (dlci-callback "callback" :type :pointer)
         (dlci-user-data-1 "userdata1" :type :pointer)
         (dlci-user-data-2 "userdata2" :type :pointer))

(cstruct adapter-info "WGPUAdapterInfo"
         (ai-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (ai-vendor "vendor" :type (:struct string-view))
         (ai-architecture "architecture" :type (:struct string-view))
         (ai-device "device" :type (:struct string-view))
         (ai-description "description" :type (:struct string-view))
         (ai-backend-type "backendType" :type backend-type)
         (ai-adapter-type "adapterType" :type adapter-type)
         (ai-vendor-id "vendorID" :type uint32t)
         (ai-device-id "deviceID" :type uint32t))

;; TODO map all
(cenum (feature-name)
       ((:wgpu-fn-depth-clip-control "WGPUFeatureName_DepthClipControl"))
       ((:wgpu-fn-undefined "CL_WGPU_UNDEFINED")))

(cstruct limits "WGPULimits"
         (limits-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (limits-max-texture-dimension-1d "maxTextureDimension1D" :type uint32t)
         (limits-max-texture-dimension-2d "maxTextureDimension2D" :type uint32t)
         (limits-max-texture-dimension-3d "maxTextureDimension3D" :type uint32t)
         (limits-max-texture-array-layers "maxTextureArrayLayers" :type uint32t)
         (limits-max-bind-groups "maxBindGroups" :type uint32t)
         (limits-max-bind-groups-plus-vertex-buffers
          "maxBindGroupsPlusVertexBuffers" :type uint32t)
         (limits-max-bidings-per-bind-group "maxBindingsPerBindGroup" :type uint32t)
         (limits-max-dynamic-uniform-buffers-per-pipeline-layout
          "maxDynamicUniformBuffersPerPipelineLayout" :type uint32t)
         (limits-max-dynamic-storage-buffers-per-pipeline-layout
          "maxDynamicStorageBuffersPerPipelineLayout" :type uint32t)
         (limits-max-sampled-textures-per-shader-stage
          "maxSampledTexturesPerShaderStage" :type uint32t)
         (limits-max-samplers-per-shader-stage "maxSamplersPerShaderStage" :type uint32t)
         (limits-max-storage-buffers-per-shader-stage
          "maxStorageBuffersPerShaderStage" :type uint32t)
         (limits-max-storage-textures-per-shader-stage
          "maxStorageTexturesPerShaderStage" :type uint32t)
         (limits-max-uniform-buffers-per-shader-stage
          "maxUniformBuffersPerShaderStage" :type uint32t)
         (limits-max-uniform-buffer-binding-size "maxUniformBufferBindingSize" :type uint64t)
         (limits-max-storage-buffer-binding-size "maxStorageBufferBindingSize" :type uint64t)
         (limits-min-uniform-buffer-offest-alignment
          "minUniformBufferOffsetAlignment" :type uint32t)
         (limits-min-storage-buffer-offset-alignment
          "minStorageBufferOffsetAlignment" :type uint32t)
         (limits-max-vertex-buffers "maxVertexBuffers" :type uint32t)
         (limits-max-buffer-size "maxBufferSize" :type uint64t)
         (limits-max-vertex-attributes "maxVertexAttributes" :type uint32t)
         (limits-max-vertex-buffer-array-stride "maxVertexBufferArrayStride" :type uint32t)
         (limits-max-inter-stage-shader-variables "maxInterStageShaderVariables" :type uint32t)
         (limits-max-color-attachments "maxColorAttachments" :type uint32t)
         (limits-max-color-attachment-bytes-per-sample
          "maxColorAttachmentBytesPerSample" :type uint32t)
         (limits-max-compute-workgroup-storage-size "maxComputeWorkgroupStorageSize" :type uint32t)
         (limits-max-compute-invocations-per-workgroup
          "maxComputeInvocationsPerWorkgroup" :type uint32t)
         (limits-max-compute-workgroup-size-x "maxComputeWorkgroupSizeX" :type uint32t)
         (limits-max-compute-workgroup-size-y "maxComputeWorkgroupSizeY" :type uint32t)
         (limits-max-compute-workgroup-size-z "maxComputeWorkgroupSizeZ" :type uint32t)
         (limits-max-compute-workgroups-per-dimension
          "maxComputeWorkgroupsPerDimension" :type uint32t))

(cstruct queue-descriptor "WGPUQueueDescriptor"
         (qd-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (qd-label "label" :type (:struct string-view)))

(cstruct device-descriptor "WGPUDeviceDescriptor"
         (dd-next "nextInChain" :type (:pointer (:struct chained-struct)))
         (dd-label "label" :type (:struct string-view))
         (dd-required-feature-count "requiredFeatureCount" :type :size)
         (dd-required-features "requiredFeatures" :type (:pointer feature-name))
         (dd-required-limits "requiredLimits" :type (:pointer (:struct limits)))
         (dd-default-queue "defaultQueue" :type (:struct queue-descriptor))
         (dd-device-lost-callback-info
          "deviceLostCallbackInfo" :type (:struct device-lost-callback-info))
         (dd-uncaptured-error-callback-info
          "uncapturedErrorCallbackInfo" :type (:struct uncaptured-error-callback-info)))

