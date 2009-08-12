;;; -*- Mode: Lisp -*-

(defpackage :executor.system
  (:use :cl :asdf))

(in-package :executor.system)

(defsystem :executor
  :depends-on (alexandria iterate semi-precious)
  :components
  ((:file "packages")
   (:file "executor" :depends-on ("packages"))))
