(defsystem #:romreader
  :name "romreader"
  :description "A library for reading various ROM formats."
  :version "0.2"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "romreader")
               (:file "nes")))
