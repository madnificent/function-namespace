(common-lisp:in-package :function-namespace.provider)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *namespaces* nil
    "Contains the namespaces which have been used in the system"))

;;;;;;;;;;;;
;;;; helpers

(defun plist-has-property-p (property plist)
  "Returns T iff <plist> has an eq equal property to <property>"
  (loop for prop in plist by #'cddr
     when (eq property prop)
     do (return-from plist-has-property-p T))
  nil)

(defun namespace-exists-p (namespace)
  "Returns T iff namespace is existant"
  (plist-has-property-p namespace *namespaces*))

;;;;;;;;;;;;;;;;;;;;
;;;; managing spaces
(defun get-space (namespace)
  "Returns the list of functions connected to their symbols in namespace <namespace>"
  (getf *namespaces* namespace))
(defun (setf get-space) (value namespace)
  (setf (getf *namespaces* namespace) value))

(defmacro create-space (namespace)
  "Sets up a namespace that doesn't exist beforehand"
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf (get-space ',namespace) nil)
     (defmacro ,namespace (function &rest args)
       (concatenate 'list
		    (list (ensure-func-name (quote ,namespace) `,function))
		    args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; managing function names
(defun get-func-name (namespace func)
  "Returns the function name of the function bound behind namespace and func"
  (getf (get-space namespace) func))
(defun (setf get-func-name) (symbol namespace func)
  (setf (getf (get-space namespace) func) symbol))

(defun ensure-func-name (namespace func &optional forced-symbol)
  "Returns the function name of the function bound behind namespace and func.  If needed, a new function is created.
 TODO: beautify me"
  (let ((current-name (get-func-name namespace func))
	(gensym-name (format nil "~A/~A/" namespace func)))
    (if current-name
	current-name
	(let ((symbol (gensym gensym-name)))
	  (import symbol)
	  (setf (get-func-name namespace func) (or forced-symbol symbol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; managing function calling

; see setup-new-space for the definition of the standard function calling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; managing function creating
(defmacro defun* ((namespace func) (&rest args) &body body)
  "Creates a new function named <func> in namespace <namespace> which takes arguments <args> and executes body <body>."
  (let ((func-name (ensure-func-name `,namespace `,func)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (setf (get-func-name (quote ,namespace) (quote ,func)) (quote ,func-name)))
       (defun ,func-name (,@args) ,@body))))

(defmacro define-compiler-macro* ((namespace func) (&rest args) &body body)
  "Creates a new function named <func> in namespace <namespace> which takes arguments <args> and executes body <body>."
  (let ((func-name (ensure-func-name `,namespace `,func)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (setf (get-func-name (quote ,namespace) (quote ,func)) (quote ,func-name)))
       (define-compiler-macro ,func-name (,@args) ,@body))))

(defmacro defmacro* ((namespace func) (&rest args) &body body)
  "Creates a new function named <func> in namespace <namespace> which takes arguments <args> and executes body <body>."
  (let ((func-name (ensure-func-name `,namespace `,func)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (setf (get-func-name (quote ,namespace) (quote ,func)) (quote ,func-name)))
       (defmacro ,func-name (,@args) ,@body))))
