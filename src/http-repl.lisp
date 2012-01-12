;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
;;
;; Copyright (C) 2012 Jong-won Choi
;; All rights reserved.
;; Distributed under the BSD-style license:
;; http://www.opensource.org/licenses/bsd-license.php
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.

;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;;; Commentary:
;;
;;
;;
;;;; Code:

(in-package :abstract5)

(defconstant +http-repl-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(defmacro define-repl-entry ((uri &optional fs-mapping) &key (if-exists :error) env reader evaluator printer)
  (let ((repl-entry (gensym "REPL-ENTRY"))
	(path (gensym "PATH"))
	(filename (gensym "FILENAME"))
	(full-pathname (when fs-mapping
			 (probe-file (format nil "~A~A" (site-home-folder *selected-site*) fs-mapping)))))
    (when fs-mapping
      (if full-pathname
 	  (setf full-pathname (namestring full-pathname))
 	  (warn "File mapping not found: ~S -> ~S" fs-mapping (format nil "~A~A" (site-home-folder *selected-site*) fs-mapping))))

    `(progn
       (assert (boundp '*selected-site*))
       (bind-if (,repl-entry (find-repl-entry ,uri))
		(cond ((eq ,if-exists :overwrite)
		       (setf (repl-entry-reader ,repl-entry) ',reader
			     (repl-entry-evaluator ,repl-entry) ',evaluator
			     (repl-entry-printer ,repl-entry) ',printer)
		       ,@(when env
			       `((setf (repl-entry-env ,repl-entry) ',env)))
		       ,@(when full-pathname
			       `((setf (repl-entry-pathname ,repl-entry) ,full-pathname)))
		       (update-records-from-instance ,repl-entry)
		       ,repl-entry)
		      ((eq ,if-exists :error)
		       (error "FIXME"))
		      (t ,repl-entry))
		(destructuring-bind (,path ,filename)
		    (split-path&name ,uri)
		  (make-db-instance 'repl-entry
				    :uri-path ,path
				    :uri-filename ,filename
				    :reader ',reader
				    :evaluator ',evaluator
				    :printer ',printer
				    ,@(when full-pathname
					    `(:pathname ,full-pathname))
				    ,@(when env
					    `(:env ',env))))))))

(site-function http-read-eval-print-loop (&optional (uri (hunchentoot:script-name*)))
  ;; FIXME add (when (maintenance-p) (find-mvc-entry "error/504"))??
  (let ((repl-entry (find-repl-entry uri))
	entry-env)
    (when (or (null repl-entry) (not (enabled-p repl-entry)))
      (setf repl-entry (find-repl-entry "error/404")))
    (with-accessors ((repl-entry-env repl-entry-env)
		     (repl-entry-pathname repl-entry-pathname)
		     (repl-entry-reader repl-entry-reader)
		     (repl-entry-evaluator repl-entry-evaluator)
		     (repl-entry-printer repl-entry-printer))
	repl-entry
      (setf entry-env repl-entry-env)
      #+XXX
      (print `(repl-entry-env ,repl-entry-env
	       repl-entry-pathname ,repl-entry-pathname
	       repl-entry-reader ,repl-entry-reader
	       repl-entry-evaluator ,repl-entry-evaluator
	       repl-entry-printer ,repl-entry-printer))
      (let ((eval-args (multiple-value-list (if repl-entry-pathname
						repl-entry-pathname
						(funcall repl-entry-reader entry-env)))))
	(cond (eval-args
	       (let ((eval-result (if repl-entry-evaluator
				      (multiple-value-list (apply repl-entry-evaluator entry-env eval-args))
				      eval-args)))
		 #+XXX
		 (print `(eval-result ,eval-result ))
		 (apply repl-entry-printer entry-env eval-result)))
	      (t (http-read-eval-print-loop "error/404")))))))

(site-function repl-entry-env-value (repl-entry key)
  (cdr (find key (repl-entry-env repl-entry))))

(site-function env-value (env key)
  (cdr (assoc key env)))

(defparameter *error-code->env-contructor*
  (list (list 404 :uri 'script-name*)))

(site-function get-error-template-and-env (env pathname)
  (bind-if (error-code (env-value env :error-code))
	   (destructuring-bind (code key fn)
	       (assoc error-code *error-code->env-contructor*)
	     (declare (ignore code))
	     (values pathname `(,key ,(funcall fn))))
	   (error "FIXME:Programming error?")))

(site-function print-template-for-error (env pathname template-env)
  (declare (ignore env))
  (with-output-to-string (out)
    (html-template:fill-and-print-template (probe-file pathname)
					   template-env
					   :stream out)))

(site-function print-static-file (env pathname)
  (declare (ignore env))
  (if (probe-file pathname)
      (hunchentoot::handle-static-file pathname )
      (http-read-eval-print-loop "error/404")))

(site-function uri-file->full-pathname (env folder)
  (declare (ignore env))
  (let ((filename (second (split-path&name (script-name*)))))
    (make-pathname :directory folder
		   :name (pathname-name filename)
		   :type (pathname-type filename))))
;;
;; Dashboard implementation
;;
(defun read-dashboard-request (env)
  (print `(,(hunchentoot:request-uri*) ,(hunchentoot:script-name*))))

(defun eval-dashboard-request (env args)
  "")

(defun print-dashboard-request (env args)
  "test")

;;; HTTP-REPL.LISP ends here
