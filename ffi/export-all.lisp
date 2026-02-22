(defmacro generate-bindings-package ()
  (let ((pkg (find-package :cl-kl-wgpu/%bindings))
        (syms '()))
    (do-symbols (sym (find-package pkg))
      (when (eql (symbol-package sym) pkg)
        (push sym syms)))
            
    `(defpackage :cl-kl-wgpu/ffi
       (:nicknames :wgpu/ffi)
       (:import-from :cl-kl-wgpu/%bindings ,@syms)
       (:export ,@syms))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (generate-bindings-package))
