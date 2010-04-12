(defpackage :function-namespace.provider
  (:use :common-lisp)
  (:export :defun*
	   :defmacro*
	   :prepare-space))

(defpackage :function-namespace
  (:use :function-namespace.provider)
  (:export :defun*))
