(defpackage cl-kl-wgpu/utils/color
  (:nicknames :wgpu/color)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:wgpu/%misc
                #:define-struct-builder)
  (:local-nicknames (#:%f #:wgpu/ffi))
  (:export
   :color
   :make-color
   :build-color
   :with-color))

(in-package :wgpu/color)

(defstruct color
  (r 0.0d0 :type double-float)
  (g 0.0d0 :type double-float)
  (b 0.0d0 :type double-float)
  (a 1.0d0 :type double-float))

(define-struct-builder build-color 'color)

(defmethod cffi:translate-into-foreign-memory ((value color) (type %f:wgpu-color-tclass) ptr)
  (cffi:with-foreign-slots ((%f:r %f:g %f:b %f:a) ptr (:struct %f:wgpu-color))
    (setf %f:r (color-r value)
          %f:g (color-g value)
          %f:b (color-b value)
          %f:a (color-a value))))

(defmacro with-color (var color &body body)
  (with-gensyms (r g b a)
    `(uiop:nest
      (cffi:with-foreign-object (,var '(:struct %f:wgpu-color)))
      (cffi:with-foreign-slots (((,r %f:r) (,g %f:g) (,b %f:b) (,a %f:a))
                                ,var (:struct %f:wgpu-color))
        (setf ,r (color-r ,color)
              ,g (color-g ,color)
              ,b (color-b ,color)
              ,a (color-a ,color))
        ,@body))))
