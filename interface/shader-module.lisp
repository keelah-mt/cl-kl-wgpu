(defpackage cl-kl-wgpu/interface/shader-module
  (:nicknames :wgpu/shader-module)
  (:use #:cl)
  (:import-from #:serapeum
                #:defunion #:match-of)
  (:import-from #:alexandria
                #:with-gensyms)
  (:local-nicknames (#:%r #:wgpu/resource)
                    (#:%f #:wgpu/ffi)
                    (#:%sv #:wgpu/%string-view)
                    (#:%cs #:wgpu/%chained-struct))
  (:export
   :shader-module
   :make-shader-module
   :shader-language
   :shader-module-source
   :shader-language-wgsl
   :shader-language-glsl
   :shader-language-spirv
   :load-source-from-file))

(in-package :wgpu/shader-module)

;; -------------------- SHADER SOURCE CODE --------------------

(defunion shader-language
  shader-language-wgsl
  shader-language-glsl
  shader-language-spirv)

(defclass shader-module-source ()
  ((source :initarg :source
           :reader source
           :type string)
   (language :initarg :language
             :reader language
             :type shader-language)))

(defun make-shader-module-source (source language)
  (declare (type string source)
           (type shader-language language))
  (make-instance 'shader-module-source :source source :language language))

(defun load-source-from-file (filespec language)
  (make-shader-module-source (uiop:read-file-string filespec) language))

;; TODO: incomplete. needs a more compact version? match-of generates a lot
(defmacro with-shader-module-source (var source &body body)
  (with-gensyms (chain code c ce)
    `(match-of shader-language (language ,source)
       (shader-language-wgsl
        (uiop:nest
         (%cs:with-chained-struct ,chain (%cs:mk-chain
                                          :s-type :wgpus-type-shader-source-wgsl))
         (%sv:with-string-view ,code (source ,source))
         (cffi:with-foreign-object (,var '%f:wgpu-shader-source-wgsl))
         (cffi:with-foreign-slots (((,c %f:chain) (,ce %f:code))
                                   ,var %f:wgpu-shader-source-wgsl)
           (setf ,c ,chain
                 ,ce ,code)
           ,@body)))
       (shader-language-glsl
        (error "TODO"))
       (shader-language-spirv
        (error "TODO")))))

;; -------------------- SHADER MODULE -------------------------

(defmacro with-shader-module-descriptor (descriptor label source &body body)
  (with-gensyms (label-view next l nin)
    `(uiop:nest
      (%sv:with-string-view ,label-view ,label)
      (with-shader-module-source ,next ,source)
      (cffi:with-foreign-object (,descriptor '%f:wgpu-shader-module-descriptor))
      (cffi:with-foreign-slots (((,l %f:label) (,nin %f:next-in-chain))
                                ,descriptor %f:wgpu-shader-module-descriptor)
        (setf ,l ,label-view
              ,nin ,next)
        ,@body))))

(defclass shader-module (%r:resource) ())

;; TODO: implement support for error scopes:
;; https://webgpu-native.github.io/webgpu-headers/Errors.html#ErrorScopes
;; it will be nicer to error out here when to see validation failure in uncaptured callback
(defun make-shader-module (device source name)
  (declare (type wgpu/device:device device)
           (type shader-module-source source)
           (type string name))
  (with-shader-module-descriptor descriptor name source
    (let ((mod (%f:wgpu-device-create-shader-module (%r:handle device) descriptor)))
      (make-instance 'shader-module :handle mod :name name))))

(defmethod %r:release ((sm shader-module))
  (%f:wgpu-shader-module-release (%r:handle sm)))

