;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:executor
  (:nicknames :exec)
  (:use :common-lisp :alexandria :iterate :pergamum)
  (:export
   ;; tunables
   #:*search-path*
   #:*execute-explanatory*
   #:*execute-verbosely*
   #:*execute-dryly*
   ;; conditions
   #:executable-failure
   #:executable-not-found
   #:required-executable-not-found
   ;; functions and macros
   #:executable
   #:find-executable
   #:with-explained-execution
   #:with-verbose-execution
   #:with-dry-execution
   #:execute-external
   #:with-explanation
   #:with-input-from-execution
   #:execution-output-string
   #:define-executable
   #:with-valid-exit-codes
   #:with-exit-code-to-error-translation
   #:exit-code-bind
   #:with-shell-predicate))
