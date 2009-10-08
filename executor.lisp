;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: EXECUTOR; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009 by
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

(in-package :executor)

(defvar *search-path* #-win32 '(#p"/usr/bin/" #p"/bin/"))

(defparameter *executables* (make-hash-table :test 'eq))

(define-root-container *executables* executable :type pathname)

(defvar *execute-verbosely* nil
  "Whether to echo the invoked external programs to standard output.")

(defvar *execute-dryly* nil
  "Whether to substitute actual execution of external programs with
   mere printing of their paths and parameters.")

(define-reported-condition executable-failure (serious-condition)
  ((program :accessor cond-program :initarg :program)
   (parameters :accessor cond-parameters :initarg :parameters)
   (status :accessor cond-status :initarg :status)
   (output :accessor cond-output :initarg :output))
  (:report (program parameters status output)
           "~@<running ~A~{ ~A~} failed with exit status ~S~:[~;, output:~@:_~:*~@<...~;~A~:@>~]~%~:@>" program parameters status output))

(define-reported-condition executable-not-found (warning)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (name search-path)
           "~@<an executable, named ~S, wasn't found in search path ~S~:@>" name search-path))

(define-reported-condition required-executable-not-found (error)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (name search-path)
           "~@<a required executable, named ~D, wasn't found in search path ~S~:@>" name search-path))

(defun find-executable (name &key (paths *search-path*) &aux (realname (string-downcase (string name))))
  "See if executable with NAME is available in PATHS. When it is, associate NAME with that path and return the latter;
   otherwise, return NIL."
  (iter (for path in paths)
        (for exec-path = (subfile path (list realname) #+win32 #+win32 :type "exe"))
        (when (probe-file exec-path) 
          (leave (setf (gethash name *executables*) exec-path)))
        (finally (warn 'executable-not-found :name realname :search-path paths))))

(defmacro with-dry-execution (&body body)
  "Execute BODY with *EXECUTE-DRYLY* bound to T."
  `(let ((*execute-dryly* t))
     (declare (special *execute-dryly*))
     ,@body))

(defmacro with-verbose-execution (&body body)
  "Execute BODY with *EXECUTE-VERBOSELY* bound to T."
  `(let ((*execute-verbosely* t))
     (declare (special *execute-verbosely*))
     ,@body))

(defun execute-external (name parameters &key (valid-exit-codes (acons 0 t nil)) translated-error-exit-codes (output nil) (environment '("HOME=/tmp"))
                         &aux (pathname (etypecase name
                                          (string (find-executable name))
                                          (pathname name)
                                          (symbol (executable name)))))
  "Run an external program at PATHNAME with PARAMETERS. 
Return a value associated with the exit code, by the means of
VALID-EXIT-CODES, or signal a condition of type EXECUTABLE-FAILURE."
  (declare (special *execute-dryly*))
  (flet ((note-execution (stream)
           (format stream ";;; ~S~{ ~S~}~%" pathname parameters)
           (finish-output stream)))
    (let* ((final-output (or output (make-string-output-stream)))
           (exit-code (progn
                        (when (or *execute-dryly* *execute-verbosely*)
                          (note-execution *standard-output*))
                        (if *execute-dryly*
                            (caar valid-exit-codes)
                            (sb-ext:process-exit-code (sb-ext:run-program pathname parameters :output final-output :environment environment))))))
      (cdr (or (assoc exit-code valid-exit-codes)
               (when-let ((error (assoc exit-code translated-error-exit-codes)))
                 (apply #'error (list* :program pathname :parameters parameters :status exit-code :output (get-output-stream-string final-output)
                                       (cdr error))))
               (error 'executable-failure :program pathname :parameters parameters :status exit-code :output (get-output-stream-string final-output)))))))

(defmacro with-input-from-execution ((stream-var name params) &body body)
  (with-gensyms (block str)
    `(block ,block
       (with-output-to-string (,str)
         (execute-external ,name ,params :output ,str)
         (with-input-from-string (,stream-var (get-output-stream-string ,str))
           (return-from ,block (progn ,@body)))))))

(defun execution-output-string (name &rest params)
  (with-output-to-string (str)
    (execute-external name params :output str)))

(defvar *valid-exit-codes* nil)
(defvar *translated-error-exit-codes* nil)

(defmacro define-executable (name &key may-want-display)
  `(progn
     (defun ,name (&rest parameters)
       (let (environment)
         (declare (ignorable environment))
         (with-retry-restarts ((retry () :report "Retry execution of the external program.")
                               (accept () :report "Accept results of external program execution as successful."
                                      (return-from ,name t))
                               ,@(when may-want-display
                                       `((retry (display)
                                                :report "Retry execution of the external program with DISPLAY set."
                                                :interactive (lambda ()
                                                               (format *query-io* "Enter value for the DISPLAY variable: ")
                                                               (finish-output *query-io*)
                                                               (list (read-line *query-io*)))
                                                (push (concatenate 'string "DISPLAY=" display) environment)))))
           (apply #'execute-external ',name parameters
                  :valid-exit-codes (acons 0 t *valid-exit-codes*)
                  :translated-error-exit-codes *translated-error-exit-codes*
                  (when environment (list :environment environment))))))))

(defmacro with-valid-exit-codes ((&rest bindings) &body body)
  `(let ((*valid-exit-codes* (list ,@(mapcar (curry #'cons 'cons) bindings))))
     ,@body))

(defmacro with-exit-code-to-error-translation ((&rest bindings) &body body)
  `(let ((*translated-error-exit-codes* (list ,@(mapcar (curry #'cons 'list) bindings))))
     ,@body))

(defmacro exit-code-bind ((&rest bindings) &body body)
  `(handler-bind ((executable-failure (lambda (cond)
                                        (case (cond-status cond)
                                          ,@bindings))))
     ,@body))

(defmacro with-shell-predicate (form)
  `(with-valid-exit-codes ((1 nil)) ,form))
