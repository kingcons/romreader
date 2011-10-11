(defsystem #:romreader
  :name "romreader"
  :description "A library for reading various ROM formats."
  :version "0.0.1"
  :license "LLGPL"
  :author "Brit Butler <redline6561@gmail.com>"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "conditions")
                             (:file "romreader")
                             (:file "nes")))))
