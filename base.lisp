(common-lisp:in-package :function-namespace.provider)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *namespaces* nil
    "Contains the namespaces which have been used in the system"))

;;;;;;;;;;;;;;;;;;;;
;;;; managing spaces
(defmacro prepare-space (name)
  "Creates a space with the name <name>"
  `(eval-when (:compile-toplevel :load-toplevel)
     (unless (get-space (quote ,name))
       (setup-new-space ,name))))

(defun get-space (namespace)
  "Returns the list of functions connected to their symbols in namespace <namespace>"
  (getf *namespaces* namespace))
(defun (setf get-space) (value namespace)
  (setf (getf *namespaces* namespace) value))

(defmacro setup-new-space (namespace)
  "Sets up a namespace that doesn't exist beforehand"
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf (get-space ',namespace) nil)
     (defmacro ,namespace (function &rest args)
       (concatenate 'list
		    (list (ensure-func-name (quote ,namespace) `,function))
		    args))))

(defun create-space (namespace)
  "Creates namespace <namespace> unless it already exists.
 TODO: needs a name-change, this isn't correct anymore"
  (get-space namespace))
(defun (setf create-space) (value namespace)
  (setf (get-space namespace) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; managing function names
(defun get-func-name (namespace func)
  "Returns the function name of the function bound behind namespace and func"
  (getf (create-space namespace) func))
(defun (setf get-func-name) (symbol namespace func)
  (setf (getf (create-space namespace) func) symbol))

(defun ensure-func-name (namespace func)
  "Returns the function name of the function bound behind namespace and func.  If needed, a new function is created."
  (let ((current-name (get-func-name namespace func))
	(gensym-name (format nil "~A/~A/" namespace func)))
    (if current-name
	current-name
	(let ((symbol (gensym gensym-name)))
	  (import symbol)
	  (setf (get-func-name namespace func) symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; managing function calling

; see setup-new-space for the definition of the standard function calling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; managing function creating
(defmacro defun* ((namespace func) (&rest args) &body body)
  "Creates a new function named <func> in namespace <namespace> which takes arguments <args> and executes body <body>."
  (let ((func-name (ensure-func-name `,namespace `,func)))
    `(defun ,func-name (,@args) ,@body)))

(defmacro define-compiler-macro* ((namespace func) (&rest args) &body body)
  "Creates a new function named <func> in namespace <namespace> which takes arguments <args> and executes body <body>."
  (let ((func-name (ensure-func-name `,namespace `,func)))
    `(define-compiler-macro ,func-name (,@args) ,@body)))

(defmacro defmacro* ((namespace func) (&rest args) &body body)
  "Creates a new function named <func> in namespace <namespace> which takes arguments <args> and executes body <body>."
  (let ((func-name (ensure-func-name `,namespace `,func)))
    `(defmacro ,func-name (,@args) ,@body)))
