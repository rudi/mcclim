
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

(defpackage :clim-mop
  (:use :mop :common-lisp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (sym :mop)
	(export sym :clim-mop))
  (export 'cl:class-name :clim-mop))
