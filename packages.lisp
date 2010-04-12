(defpackage :function-namespace.provider
  (:use :common-lisp)
  (:export :defun*
	   :defmacro*
	   :define-compiler-macro*
	   :prepare-space))

(defpackage :function-namespace
  (:use :function-namespace.provider)
  (:export :defun* :defmacro* :define-compiler-macro*))
