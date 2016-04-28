;;;; floating-point-contractions.asd

(asdf:defsystem #:floating-point-contractions
  :description "Numerically stable contractions of floating-point operations."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "floating-point-contractions")))

