(asdf:defsystem #:magical-skeleton
  :description "Magical-Skeleton is a skeleton for a 2D adventure-clicker type game."
  :author "renacava"
  :license  "TBD"
  :serial t
  :depends-on (#:clui)
  :components ((:file "package")
               (:file "main")))
