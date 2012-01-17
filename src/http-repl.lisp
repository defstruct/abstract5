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

(defun find-repl-entry-parent (parent)
  (destructuring-bind (path file)
      (split-path&name parent)
    (find-if #'(lambda (entry)
		 (and (string= (repl-entry-uri-path entry) path)
		      (string= (repl-entry-uri-filename entry) file)))
	     *repl-entries*)))

(defmacro define-repl-entry ((uri &optional pathname) &key (if-exists :error) env reader evaluator printer
			     name description parent)
  (let ((repl-entry (gensym "REPL-ENTRY")))
    (destructuring-bind (path filename)
	(split-path&name uri)
      `(progn
	 (assert (boundp '*selected-site*))
	 (bind-if (,repl-entry (find-repl-entry ,uri))
		  (cond ((eq ,if-exists :overwrite)
			 (setf (repl-entry-reader ,repl-entry) ',reader
			       (repl-entry-evaluator ,repl-entry) ',evaluator
			       (repl-entry-printer ,repl-entry) ',printer
			       (repl-entry-name ,repl-entry) ,name
			       (repl-entry-description ,repl-entry) ,description)
			 ,@(when parent
				 (setf env (repl-entry-env (find-repl-entry-parent parent)))
				 `((setf (repl-entry-parent ,repl-entry)
					 (find-repl-entry ,parent))))
			 ,@(when env
				 `((setf (repl-entry-env ,repl-entry) ',env)))
			 ,@(when pathname
				 `((setf (repl-entry-pathname ,repl-entry) ,pathname)))
			 (update-records-from-instance ,repl-entry)
			 ,repl-entry)
			((eq ,if-exists :error)
			 (error "FIXME"))
			(t ,repl-entry))
		  (push (make-db-instance 'repl-entry
					  :uri-path ,path
					  ,@(when filename
						  `(:uri-filename ,filename))

					  :reader ',reader
					  :evaluator ',evaluator
					  :printer ',printer
					  ,@(when pathname
						  `(:pathname ,pathname))
					  ,@(when env
						  `(:env ',env))
					  ,@(when name
						  `(:name ,name))
					  ,@(when description
						  `(:description ,description))
					  ,@(when parent
						  `(:parent ,(find-repl-entry parent))))
			*repl-entries*))))))
(defvar *repl-env*)

(site-function set-env-value (key val)
  (if *repl-env*
      (bind-if (found (assoc key *repl-env*))
	       (setf (cdr (last val)) (cdr found)
		     (cdr found) val)
	       (push (cons key val) *repl-env*))
      (setf *repl-env* (list (cons key val)))))

(site-function get-env-value (key)
  (cdr (assoc key *repl-env*)))

(defvar *current-repl-entry*)
(site-function http-read-eval-print-loop (&optional (uri (hunchentoot:script-name*)))
  ;; FIXME add (when (maintenance-p) (find-mvc-entry "error/504"))??
  (let ((*current-repl-entry* (find-repl-entry uri))
	*repl-env*)
    (when (or (null *current-repl-entry*) (not (enabled-p *current-repl-entry*)))
      (setf *current-repl-entry* (find-repl-entry "error/404")))
    (with-accessors ((repl-entry-env repl-entry-env)
		     (repl-entry-uri-path repl-entry-uri-path)
		     (repl-entry-uri-filename repl-entry-uri-filename)
		     (repl-entry-pathname repl-entry-pathname)
		     (repl-entry-reader repl-entry-reader)
		     (repl-entry-evaluator repl-entry-evaluator)
		     (repl-entry-printer repl-entry-printer))
	*current-repl-entry*
      (setf *repl-env* repl-entry-env)
      ;; Common env
      (set-env-value :html-template `(:html-lang ,(site-language*)
						 :charset ,(site-encoding*)
						 :title ,(format nil "~A :: ~A"
								 (site-name *selected-site*)
								 (repl-entry-name *current-repl-entry*))))      #+XXX
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
	#+XXX
	(print `(eval-args ,eval-args ,repl-entry-pathname ,(format nil "~A~A"
								    repl-entry-pathname
								    (subseq (script-name*) (length repl-entry-uri-path)))))
	(cond (eval-args
	       (let ((eval-result (if repl-entry-evaluator
				      (multiple-value-list (apply repl-entry-evaluator eval-args))
				      eval-args)))
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
  (set-env-value :html-template `(:html-body
				  ((,(merge-pathnames "html-templates/dashboard-template.html" *abstract5-home*)
				     ;; FIXME: use concrete5's translation
				     :return-to-website ,(translate "Return to Website")
				     :help ,(translate "Help")
				     :sign-out ,(translate "Sign Out")
				     :version-string ,(translate "Version")
				     :app-version "0.1"
				     ;; FIXME: add permision check
				     :nav-list ,(loop for child in (repl-entry-children *current-repl-entry*)
						   collect (list :active nil
								 :href (format nil "~A~A"
									       (repl-entry-uri-path child)
									       (repl-entry-uri-filename child))
								 :nav-name (repl-entry-name child)
								 :nav-description (repl-entry-description child)))))))
  pathname)


;;
;; /help/
;;
(site-function http-request-using-uri (pathname)
  (declare (ignore pathname))
  ;; http-request returns several vlues. Use first one only
  (nth-value 0 (drakma:http-request (get-env-value :uri))))

;;; HTTP-REPL.LISP ends here
