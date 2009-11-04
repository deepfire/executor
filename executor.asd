;;; -*- Mode: Lisp -*-

(common-lisp:defpackage :executor.system
  (:use :cl :asdf))

(common-lisp:in-package :executor.system)

(defsystem :executor
  :depends-on (:alexandria :pergamum)
  :components
  ((:file "packages")
   (:file "executor" :depends-on ("packages"))))
