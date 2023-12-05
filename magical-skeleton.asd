(asdf:defsystem #:magical-skeleton
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "magical-skeleton"
  :entry-point "magical-skeleton:start"
  :description "Magical-Skeleton is a skeleton for a 2D adventure-clicker type game."
  :author "renacava"
  :license  "TBD"
  :serial t
  :depends-on (#:clui #:deploy)
  :components ((:file "package")
               (:file "main")))
