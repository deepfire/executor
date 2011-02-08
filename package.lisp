;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:defpackage #:executor
  (:nicknames :exec)
  (:use :common-lisp :alexandria :pergamum :portable-spawn)
  (:export
   ;; tunables
   #:*search-path*
   #:*execute-explanatory*
   #:*execute-verbosely*
   #:*execute-dryly*
   ;; conditions
   #:executable-condition
   #:executable-error
   #:executable-failure
   #:executable-not-found
   #:missing-executable
   #:required-executable-not-found
   ;; base
   #:*stream-element-type*
   #:*stream-buffering*
   #:*input*
   #:*output*
   #:*error*
   #:*environment*
   #:execute
   #:execute*
   #:execute-async*
   #:pipeline
   #:pipeline*
   #:pipeline-async*
   ;; functions and macros
   #:executable
   #:find-executable
   #:with-explained-execution
   #:with-verbose-execution
   #:with-dry-execution
   #:execute-external
   #:with-explanation
   #:*executable-standard-input*
   #:*executable-standard-output*
   #:*executable-error-output*
   #:with-environment
   #:with-environment-extension
   #:define-executable
   #:pipe
   #:with-valid-exit-codes
   #:with-exit-code-to-error-translation
   #:exit-code-bind
   #:with-shell-predicate
   #:*execute-asynchronously*
   #:with-asynchronous-execution
   #:with-executable-options
   ;; remote-executor
   #:ssh
   #:compile-shell-command
   #:invoke-with-captured-external-output-and-status
   #:run-remote-commands
   #:watch-remote-commands
   ))
