(defpackage cl-kl-wgpu/interface/render-pipeline
  (:nicknames :wgpu/render-pipeline)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:wgpu/%misc
                #:define-struct-builder) 
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%c #:wgpu/common)
                    (#:%sv #:wgpu/%string-view)
                    (#:%sm #:wgpu/shader-module)
                    (#:%pl #:wgpu/pipeline-layout)
                    (#:%d #:wgpu/device))
  (:export
   :build-color-target-state
   :build-render-pipeline-descriptor
   :make-render-pipeline))

(in-package :wgpu/render-pipeline)

;; -------------------- VERTEX STATE --------------------

(defstruct (vertex-state (:constructor make-vertex))
  (module nil :type (or null %sm:shader-module))
  (entry-point "" :type string)
  (constants nil :type list)
  (buffers nil :type list))

;; TODO: handle constants and buffers
(defmacro with-vertex-state (var state &body body)
  (with-gensyms (entry-str nin sm ep c cc b bc)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-vertex-state)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                 (,sm %f:module)
                                 (,ep %f:entry-point)
                                 (,c %f:constants)
                                 (,cc %f:constant-count)
                                 (,b %f:buffers)
                                 (,bc %f:buffer-count))
                                ,var (:struct %f:wgpu-vertex-state)))
      (%sv:with-string-view ,entry-str (vertex-state-entry-point ,state)
        (setf ,nin (cffi:null-pointer)
              ,sm (%r:handle (vertex-state-module ,state))
              ,ep ,entry-str
              ,cc 0
              ,c (cffi:null-pointer)
              ,bc 0
              ,b (cffi:null-pointer))
        ,@body))))

;; -------------------- COLOR TARGET STATE --------------

(defstruct blend-component
  (operation :wgpu-blend-operation-undefined :type %c:blend-operation)
  (src-factor :wgpu-blend-factor-undefined :type %c:blend-factor)
  (dst-factor :wgpu-blend-factor-undefined :type %c:blend-factor))

(defstruct blend-state
  (color (make-blend-component) :type blend-component)
  (alpha (make-blend-component) :type blend-component))

(defstruct color-target-state
  (texture-format :wgpu-texture-format-undefined :type %c:texture-format)
  (blend-state nil :type (or null blend-state))
  (write-mask %f:wgpu-color-write-mask-all :type integer))

(define-struct-builder build-color-target-state 'color-target-state)

(defmethod cffi:translate-into-foreign-memory ((value color-target-state)
                                               (type %f:wgpu-color-target-state-tclass)
                                               ptr)
  (cffi:with-foreign-slots ((%f:next-in-chain %f:format %f:blend %f:write-mask)
                            ptr (:struct %f:wgpu-color-target-state))
    (let ((blend-state-value (color-target-state-blend-state value)))
      (setf %f:next-in-chain (cffi:null-pointer)
            %f:format (color-target-state-texture-format value)
            %f:blend (unless blend-state-value (cffi:null-pointer))
            %f:write-mask (color-target-state-write-mask value)))))

;; -------------------- FRAGMENT STATE ------------------

(defstruct (fragment-state (:constructor make-fragment))
  (module nil :type %sm:shader-module)
  (entry-point "" :type string)
  (constants nil :type list)
  (targets nil :type list))

;; TODO: handle constants
(defmacro with-fragment-state (var state &body body)
  (with-gensyms (entry-str nin sm ep c cc ts tsc targets target-count c-targets)
    `(let* ((,targets (coerce (fragment-state-targets ,state) 'vector))
            (,target-count (length ,targets)))
       (uiop:nest
        (cffi:with-foreign-object (,var '(:struct %f:wgpu-fragment-state)))
        (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                   (,sm %f:module)
                                   (,ep %f:entry-point)
                                   (,c %f:constants)
                                   (,cc %f:constant-count)
                                   (,ts %f:targets)
                                   (,tsc %f:target-count))
                                  ,var (:struct %f:wgpu-fragment-state)))
        (cffi:with-foreign-array (,c-targets
                                  ,targets
                                  `(:array (:struct %f:wgpu-color-target-state) ,,target-count)))
        (%sv:with-string-view ,entry-str (fragment-state-entry-point ,state)
          (setf ,nin (cffi:null-pointer)
                ,sm (%r:handle (fragment-state-module ,state))
                ,ep ,entry-str
                ,cc 0
                ,c (cffi:null-pointer)
                ,ts ,c-targets
                ,tsc ,target-count)
          ,@body)))))

;; -------------------- PRIMITIVE STATE -----------------

(defstruct (primitive-state (:constructor make-primitive))
  (topology :wgpu-primitive-topology-undefined :type %c:primitive-topology)
  (strip-index-format :wgpu-index-format-undefined :type %c:index-format)
  (front-face :wgpu-front-face-undefined :type %c:front-face)
  (cull-mode :wgpu-cull-mode-undefined :type %c:cull-mode)
  (unclipped-depth nil :type boolean))

(defmacro with-primitive-state (var state &body body)
  (with-gensyms (ty nin sif ff cm ud)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-primitive-state)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                 (,ty %f:topology)
                                 (,sif %f:strip-index-format)
                                 (,ff %f:front-face)
                                 (,cm %f:cull-mode)
                                 (,ud %f:unclipped-depth))
                                ,var (:struct %f:wgpu-primitive-state))
        (setf ,nin (cffi:null-pointer)
              ,ty (primitive-state-topology ,state)
              ,sif (primitive-state-strip-index-format ,state)
              ,ff (primitive-state-front-face ,state)
              ,cm (primitive-state-cull-mode ,state)
              ,ud (if (primitive-state-unclipped-depth ,state) 1 0))
        ,@body))))

;; -------------------- DEPTH STENCIL STATE ---------------------

;; TODO: there is a lot of stuff to cover, not needed now
(defstruct (depth-stencil-state (:constructor depth-stencil))
  (texture-format nil :type %c:texture-format)
  (depth-write-enabled nil :type %c:optional-bool)
  (depth-compare nil :type %c:compare-function)
  (stencil-front)
  (stencil-back)
  (stencil-read-mask)
  (stencil-write-mask)
  (depth-bias)
  (depth-bias-slope-scale)
  (depth-bias-clamp))

;; -------------------- MULTISAMPLE STATE -----------------------

(defstruct (multisample-state (:constructor make-multisample))
  (count 1 :type integer)
  (mask #xFFFFFFFF :type integer)
  (alpha-to-coverage-enabled nil :type boolean))

(defmacro with-multisample-state (var state &body body)
  (with-gensyms (nin c m ace)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-multisample-state)))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                 (,c %f:count)
                                 (,m %f:mask)
                                 (,ace %f:alpha-to-coverage-enabled))
                                ,var (:struct %f:wgpu-multisample-state))
        (setf ,nin (cffi:null-pointer)
              ,c (multisample-state-count ,state)
              ,m (multisample-state-mask ,state)
              ,ace (if (multisample-state-alpha-to-coverage-enabled ,state) 1 0)
              )
        ,@body))))

;; -------------------- RENDER PIPELINE DESCRIPTOR --------------

(defstruct render-pipeline-descriptor
  (label "" :type string)
  (layout nil :type %pl:pipeline-layout)
  (vertex (make-vertex) :type vertex-state)
  (fragment nil :type (or null fragment-state))
  (primitive (make-primitive) :type primitive-state)
  (depth-stencil nil :type (or null depth-stencil-state))
  (multisample (make-multisample) :type multisample-state))


(define-struct-builder build-render-pipeline-descriptor 'render-pipeline-descriptor)

(defmacro with-maybe-fragment-state-ptr (var state &body body)
  `(if (null ,state)
       (let ((,var (cffi:null-pointer))) ,@body)
       (with-fragment-state ,var ,state ,@body)))

;; TODO: incomplete
(defmacro with-render-pipeline-descriptor (var descriptor &body body)
  (with-gensyms (nin lbl lyt vx ft pe ds ms
                     vertex fragment primitive multisample
                     label-view vertex-state fragment-state primitive-state multisample-state)
    `(let ((,vertex (render-pipeline-descriptor-vertex ,descriptor))
           (,fragment (render-pipeline-descriptor-fragment ,descriptor))
           (,primitive (render-pipeline-descriptor-primitive ,descriptor))
           (,multisample (render-pipeline-descriptor-multisample ,descriptor)))
       (uiop:nest
        (cffi:with-foreign-object (,var '(:struct %f:wgpu-render-pipeline-descriptor)))
        (cffi:with-foreign-slots (((,nin %f:next-in-chain)
                                   (,lbl %f:label)
                                   (,lyt %f:layout)
                                   (,vx %f:vertex)
                                   (,ft %f:fragment)
                                   (,pe %f:primitive)
                                   (,ds %f:depth-stencil)
                                   (,ms %f:multisample))
                                  ,var (:struct %f:wgpu-render-pipeline-descriptor)))
        (%sv:with-string-view ,label-view (render-pipeline-descriptor-label ,descriptor))
        (with-primitive-state ,primitive-state ,primitive)
        (with-multisample-state ,multisample-state ,multisample)
        (with-vertex-state ,vertex-state ,vertex)
        (with-maybe-fragment-state-ptr ,fragment-state ,fragment
          (setf ,nin (cffi:null-pointer)
                ,lbl ,label-view
                ,lyt (%r:handle (render-pipeline-descriptor-layout ,descriptor))
                ,vx ,vertex-state
                ,ft ,fragment-state
                ,pe ,primitive-state
                ,ds (cffi:null-pointer) ;; TODO
                ,ms ,multisample-state)
          ,@body)))))

;; -------------------- RENDER PIPELINE --------------------

(defclass render-pipeline (%r:resource) ())

(defmethod %r:release ((rp render-pipeline))
  (%f:wgpu-render-pipeline-release (%r:handle rp)))

(defun make-render-pipeline (name device descriptor)
  (declare (type %d:device device)
           (type render-pipeline-descriptor descriptor))
  (with-render-pipeline-descriptor c-desc descriptor
    (let ((pipeline (%f:wgpu-device-create-render-pipeline (%r:handle device) c-desc)))
      (make-instance 'render-pipeline :name name :handle pipeline))))
