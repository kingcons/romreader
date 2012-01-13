(defsystem #:romreader
  :name "romreader"
  :description "A library for reading various ROM formats."
  :version "0.1"
  :license "LLGPL"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "conditions")
               (:file "romreader")
               (:file "nes")))
