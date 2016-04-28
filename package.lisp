;;;; package.lisp

(defpackage #:floating-point-contractions
  (:use #:cl)
  (:export
   :ln :lg :lb
   :log1+ :log1-
   :log1+/x
   :exp-1
   :exp-1/x
   :expt-1
   :log1-exp
   :log1+exp
   :log2-exp
   :logexp-1
   :hypot))
