(defpackage :cl-kl-wgpu
  (:use #:cl)
  (:nicknames :wgpu)
  (:export :with-wgpu))

(in-package :wgpu)

(defmacro with-wgpu ((wgpu-instance) &body body)
  (let ((ptr (gensym)))
    `(let ((,ptr (wgpu/%instance:create (cffi:null-pointer))))
       (unless (cffi:null-pointer-p ,ptr)
         (unwind-protect
              (let ((,wgpu-instance ,ptr))
                ,@body)
           (wgpu/%instance:release ,ptr))))))