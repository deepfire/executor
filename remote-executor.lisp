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

(in-package :executor)


(define-executable ssh)

(defun compile-shell-command (commands &aux (output ""))
  (loop :for command :in (butlast commands)
     :do
     (setf output (concatenate 'string output command " &&" #(#\Newline)))
     :finally
     (return (concatenate 'string output (lastcar commands)))))

(defun invoke-with-captured-external-output-and-status (fn external-invocation-fn)
  (let* (status
         condition
         (output
          (with-output-to-string (capture)
            (let ((*executable-standard-output-direction* capture))
              (handler-case (setf status (funcall external-invocation-fn))
                (serious-condition (c)
                  (setf condition c)))
              (when (open-stream-p capture)
                (finish-output capture))))))
    (funcall fn status output condition)))

(defun run-remote-commands (hostname username commands &optional verbose)
  (let (successp
        condition)
    (let ((output
           (with-output-to-string (capture)
             (let ((*executable-standard-output-direction* capture)
                   (commands (compile-shell-command commands)))
               (when verbose
                 (format t "~@<;;; ~@;executing following commands as user \"~A\" on ~A:~%~A~:@>~%" username hostname commands))
               (handler-case
                   (setf successp (with-input-from-string (stream commands)
                                    (with-executable-input-stream stream
                                      (ssh `(,username "@" ,hostname) "bash" "-s"))))
                 (serious-condition (c)
                   (setf condition c)))
               (when (open-stream-p capture)
                 (finish-output capture))
               (when (and verbose condition)
                 (format t "~@<;;; ~@;encountered condition while operating as user \"~A\" on ~A:~%~A~:@>~%" username hostname condition))))))
      (apply #'values
             successp
             output
             (when condition (list condition))))))

(defun watch-remote-commands (hostname username commands &optional verbose)
  (let (successp
        condition)
    (let ((output
           (with-output-to-string (capture)
             (with-pipe-stream (pipe :element-type 'character :buffering :none)
               (let ((*executable-standard-output-direction* pipe)
                     (commands (compile-shell-command commands)))
                 (when verbose
                   (format t "~&~@<;;; ~@;executing following commands as user \"~A\" on ~A:~%~A~:@>~%" username hostname commands))
                 (handler-case
                     (let ((process (with-asynchronous-execution
                                      (with-input-from-string (stream commands)
                                        (with-executable-input-stream stream
                                          (ssh `(,username "@" ,hostname) "bash" "-s"))))))
                       (declare (ignore process))
                       (close (two-way-stream-output-stream pipe))
                       (loop :for line = (read-line pipe nil nil)
                          :while line
                          :do
                          (format t "> ~A~%" line)
                          (finish-output)
                          (write-line line capture)))
                   (serious-condition (c)
                     (setf condition c)))
                 (when (and verbose condition)
                   (format t "~@<;;; ~@;encountered condition while operating as user \"~A\" on ~A:~%~A~:@>~%" username hostname condition)))))))
      (apply #'values
             successp
             output
             (when condition (list condition))))))
