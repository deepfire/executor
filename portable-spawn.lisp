;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: EXECUTOR; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(cl:defpackage #:portable-spawn
  (:use :common-lisp :pergamum)
  (:export
   ;; execution
   #:spawn-process-from-executable
   #:process-exit-code
   #:process-output
   #:process-wait
   #:process-alive-p
   ;; pipes
   #:make-pipe-stream
   #:close-pipe-read-side
   #:close-pipe-write-side
   #:with-pipe-stream))

(cl:in-package :portable-spawn)

;;;
;;; Invocation
;;;
(defun spawn-process-from-executable (pathname parameters &key input output environment (wait t))
  #+sbcl
  (sb-ext:run-program pathname parameters :input input :output output :environment environment :wait wait)
  #-(or
     sbcl)
  (not-implemented 'spawn-process-from-executable))

(defun process-exit-code (process)
  #+sbcl
  (sb-ext:process-exit-code process)
  #-(or
     sbcl)
  (not-implemented 'process-exit-code))

(defun process-output (process)
  #+sbcl
  (sb-ext:process-output process)
  #-(or
     sbcl)
  (not-implemented 'process-output))

(defun process-wait (process)
  #+sbcl
  (sb-ext:process-wait process t)
  #-(or
     sbcl)
  (not-implemented 'process-wait))

(defun process-alive-p (process)
  #+sbcl
  (sb-ext:process-alive-p process)
  #-(or
     sbcl)
  (not-implemented 'process-alive-p))

;;;
;;; Pipes
;;;
(defun make-pipe-stream (&key (element-type 'base-char) (external-format :default) (buffering :full))
  #+sbcl
  (multiple-value-bind (r w) (sb-posix:pipe)
    (make-two-way-stream
     (sb-sys:make-fd-stream r :input t :element-type element-type :external-format external-format :buffering buffering)
     (sb-sys:make-fd-stream w :output t :element-type element-type :external-format external-format :buffering buffering)))
  #-(or 
     sbcl)
  (not-implemented 'make-pipe-stream))

(defun close-pipe-read-side (pipe)
  (close (two-way-stream-input-stream pipe)))

(defun close-pipe-write-side (pipe)
  (close (two-way-stream-output-stream pipe)))

(defmacro with-pipe-stream ((s &rest make-pipe-stream-args) &body body)
  `(let ((,s (make-pipe-stream ,@make-pipe-stream-args)))
     (unwind-protect (progn ,@body)
       (close (two-way-stream-input-stream ,s))
       (close (two-way-stream-output-stream ,s)))))
