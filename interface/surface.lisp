(defpackage :cl-kl-wgpu/interface/surface
  (:nicknames :wgpu/surface)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:define-constant)
  (:import-from #:wgpu/adapter
                #:adapter)
  (:import-from #:wgpu/%misc
                #:with-zero-object
                #:make-status-error)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%cs #:wgpu/%chained-struct)
                    (#:%sv #:wgpu/%string-view))
  (:export
   :make-surface
   :release
   :get-capabilities
   :texture-usages
   :texture-formats
   :present-modes
   :alpha-modes
   :with-surface-source-xlib
   :with-surface-source-wayland))

(in-package :wgpu/surface)

;; -------------------- HELPERS --------------------

(defmacro with-surface-source-xlib ((source display window) &body body)
  (with-gensyms (xlib-chain c d w)
    `(uiop:nest
      (cffi:with-foreign-object (,source '(:struct %f:wgpu-surface-source-xlib-window)))
      (cffi:with-foreign-slots (((,c %f:chain) (,d %f:display) (,w %f:window))
                                ,source (:struct %f:wgpu-surface-source-xlib-window)))
      (%cs:with-chained-struct ,xlib-chain (%cs:mk-chain
                                            :s-type :wgpus-type-surface-source-xlib-window)
        (setf ,c ,xlib-chain
              ,d ,display
              ,w ,window)
        ,@body))))

(defmacro with-surface-source-wayland ((source display surface) &body body)
  (with-gensyms (wl-chain c d s)
    `(uiop:nest
      (cffi:with-foreign-object (,source '%f:wgpu-surface-source-wayland-surface))
      (cffi:with-foreign-slots (((,c %f:chain) (,d %f:display) (,s %f:surface))
                                ,source :wgpus-type-surface-source-wayland-surface))
      (%cs:with-chained-struct ,wl-chain (%cs:mk-chain
                                          :s-type :wgpus-type-surface-source-wayland-surface)
        (setf ,c ,wl-chain
              ,d ,display
              ,s ,surface)
        ,@body))))

;; -------------------- SURFACE CAPABILITIES -------

(define-constant +wgpu-texture-usage-map+
  `((,%f:wgpu-texture-usage-copy-src . :copy-src)
    (,%f:wgpu-texture-usage-copy-dst . :copy-dst)
    (,%f:wgpu-texture-usage-texture-binding . :texture-binding)
    (,%f:wgpu-texture-usage-storage-binding . :storage-binding)
    (,%f:wgpu-texture-usage-render-attachment . :render-attachment))
  :test 'equal)

(defun parse-wgpu-texture-usage (bitmask)
  (if (zerop bitmask)
      (list :none)
      (reduce (lambda (acc entry)
                (if (plusp (logand bitmask (car entry)))
                    (append acc (list (cdr entry)))
                    acc))
              +wgpu-texture-usage-map+
              :initial-value nil)))

(defclass surface-capabilities ()
  ((texture-usages :reader texture-usages
                   :initarg :texture-usages
                   :initform nil
                   :type list)
   (texture-formats :reader texture-formats
                    :initarg :texture-formats
                    :initform nil
                    :type list)
   (present-modes :reader present-modes
                  :initarg :present-modes
                  :initform nil
                  :type list)
   (alpha-modes :reader alpha-modes
                :initarg :alpha-modes
                :initform nil
                :type list)))

(defmethod describe-object ((caps surface-capabilities) stream)
  (format stream "~A~%" (type-of caps))
  (format stream "~%~A~20,0T~A" "KEY" "PARAM")
  (format stream "~%~A" (make-string 30 :initial-element #\-))
  (dolist (slot (closer-mop:class-direct-slots (class-of caps)))
    (let ((name (closer-mop:slot-definition-name slot)))
      (format stream "~%~A~20,0T~S" name (slot-value caps name)))))

(defmethod print-object ((caps surface-capabilities) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (caps stream)
        (with-slots (texture-usages texture-formats present-modes alpha-modes) caps
          (format stream "TU: ~X; TF: ~A; PM: ~A; AM: ~A"
                  texture-usages texture-formats present-modes alpha-modes)))))

(defun make-from-c-caps (c-caps)
  (cffi:with-foreign-slots ((%f:usages
                             %f:format-count
                             %f:formats
                             %f:present-mode-count
                             %f:present-modes
                             %f:alpha-mode-count
                             %f:alpha-modes)
                            c-caps (:struct %f:wgpu-surface-capabilities))
    (let ((usages (parse-wgpu-texture-usage %f:usages))
          (textures (cffi:foreign-array-to-lisp
                     %f:formats
                     `(:array %f:wgpu-texture-format ,%f:format-count)))
          (p-modes (cffi:foreign-array-to-lisp
                    %f:present-modes
                    `(:array %f:wgpu-present-mode ,%f:present-mode-count)))
          (a-modes (cffi:foreign-array-to-lisp
                    %f:alpha-modes
                    `(:array %f:wgpu-composite-alpha-mode ,%f:alpha-mode-count))))
      (make-instance 'surface-capabilities
                     :texture-usages usages
                     :texture-formats (coerce textures 'list)
                     :present-modes (coerce p-modes 'list)
                     :alpha-modes (coerce a-modes 'list)))))

;; -------------------- SURFACE --------------------

(defmacro with-surface-descriptor ((descriptor source &key label) &body body)
  (with-gensyms (str-view nin l)
    `(uiop:nest
      (cffi:with-foreign-object (,descriptor '%f:wgpu-surface-descriptor))
      (cffi:with-foreign-slots (((,nin %f:next-in-chain) (,l %f:label))
                                ,descriptor %f:wgpu-surface-descriptor))
      (%sv:with-string-view ,str-view ,label
        (setf ,nin ,source
              ,l ,str-view)
        ,@body))))

(defclass surface (%r:resource) ())

(defun make-surface (name wgpu source)
  (with-surface-descriptor (descriptor source :label (format nil "~A:descriptor" name))
    (let ((handle (%f:wgpu-instance-create-surface (%r:handle wgpu) descriptor)))
      (make-instance 'surface :handle handle :name name))))

(defmethod %r:release ((self surface))
  (%f:wgpu-surface-release (%r:handle self)))

(defmethod get-capabilities ((s surface) adapter)
  (declare (type adapter adapter))
  (with-zero-object (caps '%f:wgpu-surface-capabilities)
    (let ((status (%f:wgpu-surface-get-capabilities (%r:handle s)
                                                    (%r:handle adapter)
                                                    caps)))
      (if (eq status :wgpu-status-success)
          (let ((result (make-from-c-caps caps)))
            (%f:wgpu-surface-capabilities-free-members
             (cffi:mem-ref caps '(:struct %f:wgpu-surface-capabilities)))
            result)
          (error (make-status-error (%r:name s)
                                    (symbol-name '%f:wgpu-surface-get-capabilities)
                                    status))))))
