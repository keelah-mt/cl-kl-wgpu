(defpackage cl-kl-wgpu/utils/%misc
  (:nicknames :wgpu/%misc)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:format-symbol)
  (:export
   :with-zero-object
   :def-enum-type
   :build-struct
   :define-struct-builder
   :make-status-error))

(in-package :wgpu/%misc)

(defmacro with-zero-object ((var type) &body body)
  (with-gensyms (i size)
    `(cffi:with-foreign-object (,var ,type)
       (let ((,size (cffi:foreign-type-size ,type)))
         (dotimes (,i ,size)
           (setf (cffi:mem-aref ,var :unsigned-char ,i) 0))
         ,@body))))

(defmacro def-enum-type (enum-name &optional (type-name enum-name))
  "Creates a Lisp type based on the keywords of a CFFI DEFCENUM."
  `(deftype ,type-name ()
     '(member ,@(cffi:foreign-enum-keyword-list enum-name))))

(defun build-struct (type &rest args)
  "Recursively builds structs from nested keyword lists."
  (let ((constructor (format-symbol (symbol-package type) "MAKE-~A" type))
        (final-args (copy-list args)))
    (loop for (key val) on final-args by #'cddr
          do (when (and (listp val) (keywordp (first val)))
               (let ((nested-type (format-symbol (symbol-package type) "~A" key)))
                 (setf (getf final-args key)
                       (apply #'build-struct nested-type val)))))
    (apply constructor final-args)))

(defmacro define-struct-builder (name type)
  `(defun ,name (&rest config) (apply #'build-struct ,type config)))

(define-condition status-error (error)
  ((status :reader status
           :initarg :status
           :type integer)
   (resource :reader resource
             :initarg :resource
             :type string)
   (operation :reader operation
              :initarg :operation
              :type string))
  (:report (lambda (c s)
             (format s "~A failed ~A with status: ~A"
                     (resource c) (operation c) (status c)))))

(defun make-status-error (resource operation status)
  (make-instance 'adapter-info-error :resource resource :operation operation :status status))
