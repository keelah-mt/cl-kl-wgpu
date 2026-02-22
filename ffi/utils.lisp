(defpackage cl-kl-wgpu/ffi/%utils
  (:use #:cl)
  (:import-from #:cl-change-case
                #:param-case)
  (:import-from #:alexandria
                #:starts-with)
  (:export #:ffi-name-transformer
           #:import-all-owned-symbols))

(in-package :cl-kl-wgpu/ffi/%utils)

(defun set-name-case (str-name)
  "This does stuff.

And then some more stuff, it takes STR-NAME. `str-name'"
  (ecase (readtable-case *readtable*)
    (:upcase (string-upcase str-name))
    (:downcase (string-downcase str-name))
    (:preserve str-name)
    ;; TODO: ehm... no...? 
    (:invert str-name)))

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignore kind))
  (check-type name string)
  (let* ((pcase (param-case name)))
    (set-name-case (if (starts-with #\_ name)
                       (format nil "%~A" pcase)
                       pcase))))

