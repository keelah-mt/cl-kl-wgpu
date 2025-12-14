(defmacro generate-bindings-package ()
  (let ((pkg (find-package :wgpu/%grovel.exports))
        (syms '()))
    (do-all-symbols (sym (find-package pkg))
      (when (eql (symbol-package sym) pkg)
        (push sym syms)))
      
    `(defpackage :cl-kl-wgpu/bindings.grovel
       (:use #:cl #:cffi)
       (:nicknames :wgpu/%grovel)
       (:import-from :wgpu/%grovel.exports ,@syms)
       (:export ,@syms))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (generate-bindings-package))