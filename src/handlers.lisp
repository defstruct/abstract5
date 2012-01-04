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

(defconstant +handlers-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

;;;
;;; HTTP error return code handlers
;;;
(defun get-404-env ()
  `(:uri ,(hunchentoot:request-uri*)))

(defparameter *http-error-code->env-fn*
  ;; FIXME: use hash-table when it grows
  (list 404 #'get-404-env))

(defun site-error-env (http-error-code)
  (funcall (getf *http-error-code->env-fn* http-error-code)))

(site-function site-error-html-path&env (http-return-code)
  (let* ((error-code (with-output-to-string (out)
		       (princ http-return-code out)))
	 (result (select 'site-request-handler :where [and [= [slot-value 'site-request-handler 'uri-path] "error-code"]
							   [= [slot-value 'site-request-handler 'uri-filename] error-code]]
					      :flatp t)))
    (assert (null (cdr result)))
    (let* ((error-handler (first result))
	   (filename (site-request-handler-fs-filename error-handler)))

      (values
       (make-pathname :directory (format nil "~A~A" (site-home-folder *selected-site*)
					 (site-request-handler-fs-path error-handler))
		      :name (pathname-name filename)
		      :type (pathname-type filename))
       (site-error-env http-return-code)))))

(defun abstract5-http-error-handler (http-return-code)
  (multiple-value-bind (html-path env)
      (site-error-html-path&env http-return-code)
    (when html-path
      (with-output-to-string (out)
	(html-template:fill-and-print-template html-path
					       env
					       :stream out)))))
;;;
;;; static handlers
;;;
(defun %static-file-handler (site-home-folder path filename)
  (let ((pathname (make-pathname :directory (format nil "~A~A"
						    site-home-folder
						    path)
				  :name (pathname-name filename)
				  :type (pathname-type filename))))

    (if (probe-file pathname)
	(hunchentoot::handle-static-file pathname #+XXX content-type)
	(abstract5-http-error-handler 404))))

(defun static-file-handler (site-handler site filename)
  (declare (ignore filename))
  (%static-file-handler (site-home-folder site)
			(site-request-handler-fs-path site-handler)
			(site-request-handler-fs-filename site-handler)))

(defun static-folder-handler (site-handler site filename)
  (%static-file-handler (site-home-folder site)
			(site-request-handler-fs-path site-handler)
			filename))

(defparameter *default-site-request-handlers*
  '(("/js/"		"/js/")
    ("/css/"		"/css/")
    ("/images/"		"/images/")
    ("/html/"		"/html/")
    ("/favicon.ico"	"/images/favicon.ico")
    ("/robots.txt"	"/etc/robots.txt")))

(defun create-default-site-request-handlers ()
  (loop for (uri fs) in *default-site-request-handlers*
     as (uri-path uri-filename) = (split-path&name uri)
     and (fs-path fs-filename) = (split-path&name fs)
     do (cond ((and uri-filename fs-filename)
	       ;; file
	       (make-db-instance 'site-request-handler
				 :uri-path uri-path :uri-filename uri-filename
				 :fs-path fs-path   :fs-filename fs-filename
				 :fn-name "STATIC-FILE-HANDLER"))
	      ((and (null uri-filename) (null fs-filename))
	       ;; folder
	       (make-db-instance 'site-request-handler
				 :uri-path uri-path
				 :fs-path fs-path
				 :fn-name "STATIC-FOLDER-HANDLER"))
	      (t (error "Programming error - invalid spec!")))))



#.(clsql-sys:locally-enable-sql-reader-syntax)

(site-function find-site-request-handler (uri)
  (destructuring-bind (path filename)
      (split-path&name uri)
    (let ((result (select 'site-request-handler
			  :where [and [= [slot-value 'site-request-handler 'uri-path] path]
				      [or [= [slot-value 'site-request-handler 'uri-filename] filename]
					  [null [slot-value 'site-request-handler 'uri-filename]]]]
			  :flatp t)))
      (assert (null (cdr result)))
      (values (first result) filename))))

(defun exec-site-handler (site-handler site filename)
  (bind-when (fn-sym (find-symbol (site-request-handler-fn-name site-handler) :abstract5))
    (bind-when (fn (symbol-function fn-sym))
      (funcall fn
	       site-handler
	       site
	       filename))))

(defun handle-site-request (site)
  (multiple-value-bind (site-handler filename)
      (find-site-request-handler (hunchentoot:script-name*))
    (if site-handler
	(exec-site-handler site-handler site filename)
	(or (abstract5-http-error-handler 404)
	    ;; global 404 - FIXME: use custom default!
	    (hunchentoot::default-handler)))))

#.(clsql-sys:locally-disable-sql-reader-syntax)

;;; HANDLERS.LISP ends here
