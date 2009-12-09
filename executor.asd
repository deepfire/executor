;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :executor
  :depends-on (:alexandria :pergamum)
  :components
  ((:file "portable-spawn")
   (:file "package" :depends-on ("portable-spawn"))
   (:file "executor" :depends-on ("package"))
   (:file "remote-executor" :depends-on ("executor"))))
