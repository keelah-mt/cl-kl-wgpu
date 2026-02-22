(defpackage :cl-kl-wgpu/inteface/%chained-struct
  (:nicknames :wgpu/%chained-struct)
  (:use #:cl)
  (:local-nicknames (#:%f #:wgpu/ffi))
  (:export
   :with-chained-struct
   :mk-chain))

(in-package :wgpu/%chained-struct)

(defun wgpu-s-type-p (value)
  (member value (cffi:foreign-enum-keyword-list '%f:wgpus-type)))

(deftype wgpu-s-type () `(satisfies wgpu-s-type-p))

(defstruct (chained-struct (:constructor mk-chain))
  (next nil :type (or null chained-struct))
  (s-type nil :type (or null wgpu-s-type)))

(defmacro with-chained-struct (var lisp-struct &body body)
  "Recursively translates a Lisp CHAINED-STRUCT into a C-chain of 
   WGPU-CHAINED-STRUCTs, executes BODY, and cleans up all memory."
  (let ((stack-var (gensym "ALLOCATED-PTRS")))
    `(let ((,stack-var '()))
       (labels ((translate-node (node)
                  (if (null node)
                      (cffi:null-pointer)
                      (let ((ptr (cffi:foreign-alloc '(:struct %f:wgpu-chained-struct))))
                        (push ptr ,stack-var)
                        (setf (cffi:foreign-slot-value ptr
                                                       '(:struct %f:wgpu-chained-struct)
                                                       '%f:s-type)
                              (chained-struct-s-type node))
                        (setf (cffi:foreign-slot-value ptr
                                                       '(:struct %f:wgpu-chained-struct)
                                                       '%f:next)
                              (translate-node (chained-struct-next node)))
                        ptr))))
         (let ((,var (translate-node ,lisp-struct)))
           (unwind-protect
                (progn ,@body)
             (dolist (p ,stack-var)
               (cffi:foreign-free p))))))))

