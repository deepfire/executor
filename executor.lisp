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

(defvar *executables* (make-hash-table :test 'eq))

(define-root-container *executables* executable :type pathname)

(defvar *execute-explanatory* nil
  "Whether to print provided explanations while executing.")

(defvar *execute-verbosely* nil
  "Whether to echo the invoked external programs to standard output.
Implies *EXECUTE-EXPLANATORY*.")

(defvar *execute-dryly* nil
  "Whether to substitute actual execution of external programs with
mere printing of their paths and parameters.
Implies *EXECUTE-VERBOSELY*")

(define-condition executable-condition () ())
(define-condition executable-error (executable-condition error) ())

(define-reported-condition executable-failure (executable-error)
  ((program :accessor cond-program :initarg :program)
   (parameters :accessor cond-parameters :initarg :parameters)
   (status :accessor cond-status :initarg :status)
   (working-directory :accessor cond-working-directory :initarg :working-directory)
   (output :accessor cond-output :initarg :output))
  (:report (program parameters working-directory status output)
           "~@<Running ~A~{ ~S~} in working-directory ~S failed with exit status ~S, ~:[~;, output:~%~:*~@<>>>~;~A~:@>~]~:@>" program parameters working-directory status output))

(define-reported-condition missing-executable (executable-error)
  ((name :accessor cond-name :initarg :name))
  (:report (name)
           "~@<Executable named ~S wasn't present on the system.~:@>" name))

(define-reported-condition executable-not-found (executable-condition warning)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (name search-path)
           "~@<An executable, named ~S, wasn't found in search path ~S.~:@>" name search-path))

(define-reported-condition required-executable-not-found (executable-error)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (name search-path)
           "~@<A required executable, named ~D, wasn't found in search path ~S.~:@>" name search-path))

(defun find-executable (name &key (if-does-not-exist :warn) (paths *search-path*) &aux (realname (string-downcase (string name))))
  "See if executable with NAME is available in PATHS. When it is, associate NAME
with that path and return the latter;  otherwise, proceed according to the value
of IF-DOES-NOT-EXIST:
  :CONTINUE - return NIL;
  :WARN     - signal a warning of type EXECUTABLE-NOT-FOUND;
  :ERROR    - signal an error of type EXECUTABLE-NOT-FOUND."
  (dolist (path paths)
    (let ((exec-path (subfile path (list realname) #+win32 #+win32 :type "exe")))
      (when (probe-file exec-path) 
        (return-from find-executable (setf (gethash name *executables*) exec-path)))))
  (ecase if-does-not-exist
    (:continue)
    (:warn (warn 'executable-not-found :name realname :search-path paths))
    (:error (error 'executable-not-found :name realname :search-path paths))))


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

(defmacro with-explained-execution (&body body)
  "Execute BODY with *EXECUTE-EXPLANATORY* bound to T."
  `(let ((*execute-explanatory* t))
     (declare (special *execute-explanatory*))
     ,@body))

(defvar *stream-element-type* 'character)
(defvar *stream-buffering* :none)
(defvar *input* nil)
(defvar *output* t)
(defvar *error* :output)
(defvar *environment* '("HOME=/tmp"))
(defvar *time-limit* nil)

(defun make-executable-pipe-stream (&key (element-type *stream-element-type*) (buffering *stream-buffering*))
  (make-pipe-stream :element-type element-type :buffering buffering))

(defun interpret-output-stream-designator (stream)
  (if (eq t stream)
      *standard-output*
      stream))

(defun %execute (pathname parameters asyncp)
  (let ((process (spawn-process-from-executable pathname parameters :wait nil
                                                :environment *environment*
                                                :input *input*
                                                :output (interpret-output-stream-designator *output*)
                                                :error *error*)))
    (cond (asyncp process)
          (t
           (let (timer)
             (unwind-protect
                  (progn
                    (when *time-limit*
                      (setf timer (schedule-timer
                                   (make-timer (lambda ()
                                                 (when (process-alive-p process)
                                                   (process-kill process sigkill))))
                                   *time-limit*)))
                    (process-wait process)
                    (process-exit-code process))
               (when *time-limit*
                 (unschedule-timer timer))))))))

(defun execute (pathname parameters)
  (%execute pathname parameters nil))

(defun execute-async (pathname parameters)
  (%execute pathname parameters t))

(defun execute* (pathname &rest parameters)
  (execute pathname parameters))

(defun execute-async* (pathname &rest parameters)
  (execute-async pathname parameters))

(defun pipeline (commands &key async)
  (labels ((rec (commands input)
             (destructuring-bind (&optional command &rest more-commands) commands
               (destructuring-bind (&optional pathname &rest parameters) command
                 (when pathname
                   (let* ((output (if more-commands
                                      (make-executable-pipe-stream)
                                      (interpret-output-stream-designator *output*)))
                          (process (spawn-process-from-executable
                                    pathname parameters
                                    :input input :output output :error *error* :environment *environment* :wait nil)))
                     (when more-commands
                       (close-pipe-write-side output))
                     (unless (eq input *input*)
                       (close-pipe-read-side input))
                     (cons process (rec more-commands output))))))))
    (when commands
      (let ((processes (rec commands *input*)))
        (cond (async
               processes)
              (t
               (process-wait (lastcar processes))
               (process-exit-code (lastcar processes))))))))

(defun pipeline* (&rest commands)
  (pipeline commands :async nil))

(defun pipeline-async* (&rest commands)
  (pipeline commands :async t))

(defun execute-external (name parameters &key (valid-exit-codes (acons 0 t nil)) (wait t) translated-error-exit-codes (output nil) input (environment '("HOME=/tmp"))
                         explanation
                         &aux (pathname (etypecase name
                                          (string (find-executable name))
                                          (pathname name)
                                          (symbol (or (executable name :if-does-not-exist :continue)
                                                      (error 'missing-executable :name name))))))
  "Run an external program at PATHNAME with PARAMETERS. 
Return a value associated with the exit code, by the means of
VALID-EXIT-CODES, or signal a condition of type EXECUTABLE-FAILURE.
OUTPUT should be either a stream, T, NIL or :CAPTURE, with
following interpretation of the latter three:
   T - *STANDARD-OUTPUT*,
   NIL - /dev/null, nul or whatever is the name of the local void,
   :CAPTURE - capture into a string."
  (when (or *execute-explanatory* *execute-verbosely* *execute-dryly*)
    (destructuring-bind (format-control &rest format-arguments) (ensure-cons explanation)
      (apply #'format *standard-output* (concatenate 'string "~@<;;; ~@;" format-control "~:@>~%") format-arguments)))
  (when (or *execute-verbosely* *execute-dryly*)
    (format *standard-output* "~@<;;; ~@;~S '~S :environment '~S :output ~S~:@>~%" pathname parameters environment output)
    (finish-output *standard-output*))
  (multiple-value-bind (final-output capturep) (if (streamp output)
                                                   output
                                                   (case output
                                                     ((t) *standard-output*)
                                                     ((nil) nil)
                                                     (:capture (values (make-string-output-stream) t))
                                                     (t (error "~@<Bad OUTPUT passed to EXECUTE-EXTERNAL: ~
                                                                   should be either a stream, or one of (T NIL :CAPTURE).~:@>"))))
    (if *execute-dryly*
        (values (cdar valid-exit-codes)
                (when capturep ""))
        (let ((*input* input)
              (*output* final-output)
              (*environment* environment))
          (if wait
              (let ((working-directory (posix-working-directory))
                    ;; There's a potential race component here.  Nothing we can deal with, though.
                    (exit-code (execute pathname parameters)))
                (apply #'values
                       (cdr (or (assoc exit-code valid-exit-codes)
                                (let ((error-output (if (or capturep (and (typep final-output 'string-stream) (output-stream-p final-output)))
                                                        (get-output-stream-string final-output) "#<not captured>")))
                                  (destructuring-bind (type &rest error-initargs) (if-let ((error (assoc exit-code translated-error-exit-codes)))
                                                                                    (rest error)
                                                                                    '(executable-failure))
                                    (apply #'error type (list* :program pathname :parameters parameters :status exit-code :output error-output :working-directory working-directory
                                                               error-initargs))))))
                       (when capturep
                         (list (get-output-stream-string final-output)))))
              (execute-async pathname parameters))))))

(defvar *valid-exit-codes* nil)
(defvar *translated-error-exit-codes* nil)
(defvar *environment* '("HOME=/tmp"))
(defvar *explanation* '("<unexplained action>"))
(defvar *executable-standard-output-direction* :capture)
(defvar *executable-input-stream* nil)
(defvar *execute-asynchronously* nil)

(defmacro with-explanation (explanation &body body)
  "Execute BODY with *EXPLANATION* bound to EXPLANATION."
  `(let ((*explanation* ,(if (consp explanation) `(list ,@explanation) explanation)))
     ,@body))

(define-execute-with-bound-variable *executable-standard-output-direction*
  (:binding unaffected-executable-output t :define-with-maybe-macro t :documentation
            "Execute BODY without capturing standard output from executables.")
  (:binding captured-executable-output :capture :define-with-maybe-macro t :documentation
            "Execute BODY while capturing standard output from executables.")
  (:binding avoided-executable-output nil :define-with-maybe-macro t :documentation
            "Execute BODY while avoiding standard output from executables."))

(defmacro with-environment (environment &body body)
  "Execute BODY with process variable environment set to ENVIRONMENT."
  `(let ((*environment* ,environment))
     ,@body))

(defmacro with-environment-extension (extension &body body)
  "Execute BODY with process variable environment prepended with EXTENSION."
  `(let ((*environment* (append ,extension *environment*)))
     ,@body))

(defmacro with-executable-input-stream (stream &body body)
  "Execute BODY with process input set to STREAM."
  `(let ((*executable-input-stream* ,stream))
     ,@body))

(defmacro with-asynchronous-execution (&body body)
  "Execute BODY within dynamic extent in which all calls to EXECUTE-EXTERNAL
immediately return a process structure, without waiting for the process
to finish."
  `(let ((*execute-asynchronously* t))
     ,@body))

(defun process-arg (arg)
  (etypecase arg
    (pathname (namestring arg))
    (list (apply #'concatenate 'string (mapcar #'process-arg arg)))
    (string arg)))

(defmacro define-executable (name &key may-want-display fixed-environment)
  (destructuring-bind (function-name &optional (executable-name function-name)) (ensure-cons name)
    `(defun ,function-name (&rest parameters)
       (let ((environment ,(if fixed-environment `(remove nil (list ,@fixed-environment)) '*environment*)))
         (with-retry-restarts ((retry () :report "Retry execution of the external program.")
                               (accept () :report "Accept results of external program execution as successful. Return T."
                                       (return-from ,function-name t))
                               (fail () :report "Accept results of external program execution as failure. Return NIL."
                                     (return-from ,function-name nil))
                               ,@(when may-want-display
                                       `((retry (display)
                                                :report "Retry execution of the external program with DISPLAY set."
                                                :interactive (lambda ()
                                                               (format *query-io* "Enter value for the DISPLAY variable: ")
                                                               (finish-output *query-io*)
                                                               (list (read-line *query-io*)))
                                                (push (concatenate 'string "DISPLAY=" display) environment)))))
           (apply #'execute-external ',executable-name (mapcar #'process-arg parameters)
                  :explanation (when (boundp '*explanation*) *explanation*)
                  :valid-exit-codes (acons 0 t *valid-exit-codes*)
                  :translated-error-exit-codes *translated-error-exit-codes*
                  :wait (not *execute-asynchronously*)
                  :input *executable-input-stream*
                  :output *executable-standard-output-direction*
                  (when environment (list :environment environment))))))))

(defmacro pipe (&rest commands)
  `(pipeline (list ,@(loop :for (name . args) :in commands
                        :collect `(list* (executable ',name) (mapcar #'process-arg (list ,@args)))))))

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
