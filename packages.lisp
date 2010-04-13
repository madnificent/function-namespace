(defpackage :function-namespace.provider
  (:use :common-lisp)
  (:export :defun*
	   :defmacro*
	   :define-compiler-macro*
	   :create-space))

(defpackage :function-namespace
  (:use :function-namespace.provider)
  (:export :defun* :defmacro* :define-compiler-macro*))
