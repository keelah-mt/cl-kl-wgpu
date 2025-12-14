(defpackage cl-kl-wgpu/bindings.utils
  (:use #:cl)
  (:nicknames :wgpu/%utils)
  (:import-from :cffi
                #:with-foreign-string
                #:with-foreign-object
                #:with-foreign-slots
                #:foreign-string-to-lisp
                #:translate-from-foreign
                #:translate-to-foreign
                #:translate-into-foreign-memory
                #:free-translated-object)
  (:local-nicknames (:%g :wgpu/%grovel))
  (:export :make-chained-struct
           :chained-struct-convert
           :make-string-view
           :string-view-convert))

(in-package :wgpu/%utils)

(defclass string-view ()
  ((str :reader sv-str :initarg :str :type string)))

(defun make-string-view (str)
  (make-instance 'string-view :str str))

(defgeneric string-view-convert (string-view)
  (:method ((str string))
    (cffi:convert-to-foreign str '(:struct %g:string-view)))
  (:method ((self string-view))
    (cffi:convert-to-foreign (sv-str self) '(:struct %g:string-view))))

;; TODO: test null/count handling
(defmethod translate-from-foreign (v (type %g:string-view-tclass))
  (with-foreign-slots ((%g:sv-data %g:sv-data-length) v (:struct %g:string-view))
    (cffi:foreign-string-to-lisp %g:sv-data
                                 :count (when (/= %g:sv-data-length %g:sv-null-cstring)
                                          %g:sv-data-length))))

(defmethod translate-into-foreign-memory (v (type %g:string-view-tclass) pointer)
  (with-foreign-slots ((%g:sv-data %g:sv-data-length) pointer (:struct %g:string-view))
    (if v
      (let ((str (cffi:foreign-string-alloc v :null-terminated-p nil)))
        (setf %g:sv-data str
              %g:sv-data-length (length v)))
      (setf %g:sv-data (cffi:null-pointer)
            %g:sv-data-length 0))))

;; TODO: not called?
(defmethod free-translated-object (v (type %g:string-view-tclass) param)
  (declare (ignore param))
  (with-foreign-slots ((%g:sv-data) v (:struct %g:string-view))
    (unless (cffi:null-pointer-p %g:sv-data)
      (cffi:foreign-string-free %g:sv-data))))

(defclass chained-struct ()
  ((type :reader cs-type
         :initarg :type
         :type %g:s-type)
   (next :reader cs-next
         :initarg :next
         :type (or chained-struct nil))))

(defgeneric chained-struct-convert (chained-struct)
  (:method ((self chained-struct))
    (cffi:convert-to-foreign self '(:struct %g:chained-struct))))

(defmethod print-object ((self chained-struct) s)
  (if *print-readably*
      (call-next-method)
      (labels ((get-inner (cs) (with-slots (type next) cs
                                 (format nil "(~A ~A)" type (when next (get-inner next))))))
        (print-unreadable-object (self s :identity t :type t)
          (format s "~A" (get-inner self))))))

(defun make-chained-struct (&key type (next nil))
  (make-instance 'chained-struct :type type :next next))

(defmethod translate-from-foreign (pointer (type %g:chained-struct-tclass))
  (with-foreign-slots ((%g:cs-next %g:cs-type) pointer (:struct %g:chained-struct))
    (make-chained-struct :type %g:cs-type
                         :next (unless (cffi:null-pointer-p %g:cs-next)
                                 (cffi:convert-from-foreign %g:cs-next
                                                            '(:struct %g:chained-struct))))))

(defmethod translate-into-foreign-memory ((v chained-struct)
                                          (type %g:chained-struct-tclass)
                                          pointer)
  (with-foreign-slots ((%g:cs-next %g:cs-type) pointer (:struct %g:chained-struct))
    (with-slots (type next) v
      (setf %g:cs-next (if next
                           (cffi:convert-to-foreign next '(:struct %g:chained-struct))
                           (cffi:null-pointer))
            %g:cs-type type))))
