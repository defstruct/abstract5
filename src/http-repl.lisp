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

(defmacro define-repl-entry ((uri &optional pathname) &key (if-exists :error) env reader evaluator printer)
  (let ((repl-entry (gensym "REPL-ENTRY"))
	(path (gensym "PATH"))
	(filename (gensym "FILENAME")))
    `(progn
       (assert (boundp '*selected-site*))
       (bind-if (,repl-entry (find-repl-entry ,uri))
		(cond ((eq ,if-exists :overwrite)
		       (setf (repl-entry-reader ,repl-entry) ',reader
			     (repl-entry-evaluator ,repl-entry) ',evaluator
			     (repl-entry-printer ,repl-entry) ',printer)
		       ,@(when env
			       `((setf (repl-entry-env ,repl-entry) ',env)))
		       ,@(when pathname
			       `((setf (repl-entry-pathname ,repl-entry) ,pathname)))
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
				    ,@(when pathname
					    `(:pathname ,pathname))
				    ,@(when env
					    `(:env ',env))))))))
(defvar *repl-env*)

(site-function set-env-value (key val)
  (if *repl-env*
      (bind-if (found (assoc key *repl-env*))
	       (setf (cdr found) val)
	       (push (cons key val) *repl-env*))
      (setf *repl-env* (list (cons key val)))))

(site-function get-env-value (key)
  (cdr (assoc key *repl-env*)))

(site-function http-read-eval-print-loop (&optional (uri (hunchentoot:script-name*)))
  ;; FIXME add (when (maintenance-p) (find-mvc-entry "error/504"))??
  (let ((repl-entry (find-repl-entry uri))
	*repl-env*)
    (when (or (null repl-entry) (not (enabled-p repl-entry)))
      (setf repl-entry (find-repl-entry "error/404")))
    (with-accessors ((repl-entry-env repl-entry-env)
		     (repl-entry-uri-path repl-entry-uri-path)
		     (repl-entry-uri-filename repl-entry-uri-filename)
		     (repl-entry-pathname repl-entry-pathname)
		     (repl-entry-reader repl-entry-reader)
		     (repl-entry-evaluator repl-entry-evaluator)
		     (repl-entry-printer repl-entry-printer))
	repl-entry
      (setf *repl-env* repl-entry-env)
      #+XXX
      (print `(repl-entry-env ,repl-entry-env
	       repl-entry-pathname ,repl-entry-pathname
	       repl-entry-reader ,repl-entry-reader
	       repl-entry-evaluator ,repl-entry-evaluator
	       repl-entry-printer ,repl-entry-printer))
      (let ((eval-args (multiple-value-list (cond ((and repl-entry-uri-path repl-entry-uri-filename)
						   ;; file mapping
						   repl-entry-pathname)
						  (repl-entry-uri-path
						   ;; dir mapping
						   (format nil "~A~A"
							   repl-entry-pathname
							   (subseq (script-name*) (length repl-entry-uri-path))))
						  (t (funcall repl-entry-reader))))))
	;;(print `(eval-args ,eval-args ))
	(cond (eval-args
	       (let ((eval-result (if repl-entry-evaluator
				      (multiple-value-list (apply repl-entry-evaluator eval-args))
				      eval-args)))
		 #+XXX
		 (print `(eval-result ,eval-result ))
		 (apply repl-entry-printer eval-result)))
	      (t (http-read-eval-print-loop "error/404")))))))

(defparameter *error-code->env-contructor*
  (list (list 404 :uri 'script-name*)))

(site-function set-env/get-error-template (pathname)
  (bind-if (error-code (get-env-value :error-code))
	   (destructuring-bind (code key fn)
	       (assoc error-code *error-code->env-contructor*)
	     (declare (ignore code))
	     (set-env-value :html-template (list key (funcall fn)))
	     pathname)
	   (error "FIXME:Programming error?")))

(defun print-standard-html-template (filename)
  ;; FIXME: add theme dir later
  (loop for folder in (list #+FIXME *selected-theme*
			    (and (boundp '*selected-site*) (site-home-folder *selected-site*))
			    *abstract5-home*)
     as merged-pathname = (merge-pathnames filename folder)
     when (setf merged-pathname (probe-file merged-pathname))
     do (return-from print-standard-html-template
	  (with-output-to-string (out)
	    (html-template:fill-and-print-template merged-pathname
						   (get-env-value :html-template)
					      :stream out))))
  (error "Requested template ~S not found in theme, site and global folders" filename))

(site-function print-static-file (pathname)
  (if (and pathname (probe-file pathname))
      (hunchentoot::handle-static-file pathname )
      (http-read-eval-print-loop "error/404")))

(site-function uri-file->full-pathname (relative-pathname)
  (loop for folder in (list #+FIXME *selected-theme*
			      (and (boundp '*selected-site*) (site-home-folder *selected-site*))
			      *abstract5-home*)
     as merged-pathname = (format nil "~A~A" folder relative-pathname)
     when (setf merged-pathname (probe-file merged-pathname))
     do (return-from uri-file->full-pathname
	  merged-pathname)))
;;
;; Dashboard implementation
;;
(defun eval-dashboard-request (pathname)
  (let ((css-files (mapcar (lambda (css)
			       (list :css-file (format nil "/css/~A" css)))
			     '("ccm.base.css"

			       "ccm.dashboard.css" "ccm.colorpicker.css" "ccm.menus.css"
			       "ccm.forms.css" "ccm.search.css" "ccm.filemanager.css" "ccm.dialog.css"
			       "jquery.rating.css" "jquery.ui.css")))
	(js-files (mapcar (lambda (js)
			    (list :js-file (format nil "/js/~A" js)))
			  '("jquery.js" "ccm.base.js"
			    "jquery.ui.js" "ccm.dialog.js" "ccm.base.js" "jquery.rating.js"
			    "jquery.form.js" "ccm.ui.js" "quicksilver.js"
			    "jquery.liveupdate.js" "ccm.search.js" "ccm.filemanager.js"
			    "ccm.themes.js" "jquery.ui.js" "jquery.colorpicker.js" "tiny_mce/tiny_mce.js"
			    ))))
    (set-env-value :html-template `(:html-lang "en" :charset "utf-8"
					       :title ,(format nil "~A :: ~A" (site-name *selected-site*) "Dashboard")
					       :css-files ,css-files
					       :js-files ,js-files)))
  pathname)

;;; HTTP-REPL.LISP ends here
