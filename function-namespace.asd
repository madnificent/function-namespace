(asdf:defsystem :function-namespace
  :name "function-namespace"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "A system which allows you to use and create alternative namespaces"
  :depends-on ()
  :components ((:file "packages")
	       (:file "base" :depends-on ("packages"))))
