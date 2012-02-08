;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
;;
;; Copyright (C) 2011 Jong-won Choi
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

(defconstant +abstract5-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstrutc.com")

(defun request->subdomain (http-request)
  (let ((host-name (hunchentoot:host http-request)))
    (subseq host-name 0 (or (position #\. host-name)
			    (position #\: host-name)))))

#.(locally-enable-sql-reader-syntax)

(defun installation-page ()
  (with-http-parameters ((email "email") (password1 "password1") (password2 "password2"))
    (let ((install.html (make-pathname :directory (cdr (assoc :html *abstract5-home-folders*))
				       :name "install.html"))
	  (messages ()))
      (with-output-to-string (html-out)
	(cond ((or email password1 password2)
	       (unless (valid-email-p email)
		 (push "Invalid Email address!" messages))
	       (unless (>= (length password1) 8)
		 (push "Password must be more than 8 characters!" messages))
	       (cond (messages
		      `(:message ,(format nil "~{~A~&~}" (nreverse messages))))
		     ((string= password1 password2)
		      (insert-records :into [admin]
				      :attributes '(password password_salt email)
				      :values (let ((salt (make-random-salt)))
						(list (encode-password password1 salt) salt email)))
		      (html-template:fill-and-print-template install.html
							     '(:message "Added system admin account.")
							     :stream html-out)
		      (hunchentoot:redirect "/admin.html"))
		     (t (html-template:fill-and-print-template install.html '(:message "Password does not match!")
							       :stream html-out))))
	      (t (html-template:fill-and-print-template install.html ()
							:stream html-out)))))))

(defun admin.html ()
  (with-http-parameters ((email "email") (password0 "password0") (password0 "password0"))
    ))

(defun admin-sites.html ()
  )

(defvar *script-name->env-fn-mapping*
  '(("/admin.html" . admin.html)
    ("/admin-sites.html" . admin-sites.html)))

(defun admin-page ()
  (let ((entry (assoc (hunchentoot:script-name*) *script-name->env-fn-mapping* :test #'string=)))
    (with-output-to-string (html-out)
      (if entry
	  (html-template:fill-and-print-template (make-pathname :directory (cdr (assoc :html *abstract5-home-folders*))
								:name (car entry))
						 (funcall (cdr entry))
						 :stream html-out)
	  (html-template:fill-and-print-template (make-pathname :directory (cdr (assoc :html *abstract5-home-folders*))
								:name "page-not-found"
								:type "html")
						 `(:uri ,(HUNCHENTOOT:REQUEST-URI*))
						 :stream html-out)))))

(defun process-admin-request ()
  (if (select [*] :from [admin] :flatp t)
      (admin-page)
      (installation-page)))

(defun system-admin-request-p ()
  (let* ((domain (hunchentoot:host))
	 (colon-pos (position #\: domain)))
    (some #'(lambda (local)
	      (string= (subseq domain 0 (or colon-pos (length local))) local))
	  '("127.0.0.1" "localhost"))))

(defmacro with-site-context ((var domain) &body body)
  `(let ((*package* (find-package :abstract5)))
     (bind-if (*selected-site* (find-site-from-subdomain-name ,domain))
	      (let ((,var *selected-site*))
		(on-schema ((site-db-schema *selected-site*))
		  ,@body))
	      (if (system-admin-request-p)
		  (process-admin-request)
		  (error 'site-not-found :domain ,domain)))))


(defun http-request-handler (request)
  #+XXX
  (print `(host ,(hunchentoot:host)
	   remote-addr ,(hunchentoot:remote-addr request)
		       remote-port ,(hunchentoot:remote-port request)
		       request-method ,(hunchentoot:request-method request)
		       request-uri ,(hunchentoot:request-uri request)
		       server-protocol ,(hunchentoot:server-protocol request)
		       script-name ,(hunchentoot:script-name request)
		       real-remote-addr ,(hunchentoot:real-remote-addr request)
		       user-agent ,(hunchentoot:user-agent )
		       ))
  (clsql-postgresql-socket::with-connection-from-pool
    (with-site-context (site (request->subdomain request))
      (http-read-eval-print-loop))))

#+XXX
(defun create-system-admin ()
  (loop with password
     do
       (princ "Please enter password for the admin: " *query-io*)
       (setf password (read-line *query-io*))
       (terpri *query-io*)
       (princ "Please enter password, again: " *query-io*)
     if (string= password (read-line *query-io*))
     do (print password)
       (insert-records :into [admin]
		       :attributes '(password password_salt)
		       :values (let ((salt (make-random-salt)))
				 (list (encode-password password salt) salt)))
       (return)
     else
     do
       (terpri *query-io*)
       (princ "Password mismatched! Please try again! " *query-io*)
       (loop repeat 4
	  do (terpri *query-io*))))

(defun main ()
  ;; check config file. Load it or start installation web page
  ;; DB(?) when?
  ;; check update/migration file. Load and start migration
  ;; check patch files. Load them

  ;; timezone, session,
  ;; FIXME: read from config file
  (clsql-sys:connect '(#P"/var/run/postgresql/.s.PGSQL.5432" "abstract5" "jc" nil 5432) :encoding :utf-8)
  (clsql-postgresql-socket::with-connection-from-pool
    #+XXX
    (unless (select [*] :from [admin] :flatp t)
      (create-system-admin)
      (ccl:quit))
    (init-public-sql))
  (setf hunchentoot:*hunchentoot-default-external-format* hunchentoot::+utf-8+)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080)))

#.(locally-disable-sql-reader-syntax)

;;;
;;; Replace Hunchentoot's dispatcher function
;;;
(in-package :hunchentoot)

(defun list-request-dispatcher (request)
  (abstract5::http-request-handler request))


#|



(defun mvc-message-map (arg)
  (declare (ignore arg))
  "FIXME")

(defun get-mvc-message (&rest keys)
  "Get the message with given keys under *mvc-site*"
  (let ((message (or (gethash (mvc-message-map *mvc-site*) keys)
		     #+XXX
		     (gethash (mvc-message-map *root-site*) keys))))
    (unless message
      (error "Programming Error! GET-MVC-MESSAGE does not have the entry for ~S" keys))
    message))


;; FIXME: move to proper place
(defun get-site-environment ()
  '(:html-lang "abc"
    ;; FIXME: get actual values from site object *mvc-site*
    :theme-html-header (("site-instances/root/themes/core/header-view.html"
			 ;; FIXME: establish site based css, img, etc directories
			 :charset "UTF-8"
			 :title   "Login"
			 :meta-description "Some description"
			 :meta-keywords "key1 key2 etc"
			 ;; :meta-content-lang "en-AU"
			 :theme-css-files ((:css-file "/site-instances/root/themes/core/css/ccm.default.theme.css")
					   (:css-file "/site-instances/root/themes/core/css/ccm.base.css"))
			 :theme-js-files ((:js-file "/site-instances/root/themes/core/js/jquery.js")
					  (:js-file "/site-instances/root/themes/core/js/ccm.base.js"))
			 ))
    :theme-html-body (("site-instances/root/themes/core/body-view.html"
		       ;; FIXME: establish site based css, img, etc directories
		       :site-logo "/a/b/c/logo.png"
		       :site-logo-alt ()
		       :errors ())
		      ("site-instances/root/mvc/html-views/plain-login.html"
		       :site-name "example-site"))
    :theme-css-files ((:css-file "ccm.default.theme.css"))
	      ;; no :extra-header
    :mvc-errors (get-mvc-errors)))

(defun get-global-environment ()
  `(:html-lang #+FIXME ,(get-site-html-lang)))

(defmacro with-mvc-site-env ((mvc) &body body)
  `(let ((*database* (switch-db-schema "FIXME"))
	 (*mvc-errors* ())
	 (*mvc-site* (find-mvc-site??? "FIXME")))
     ,@body))


;;;
;;; with-site-db, with-site-env, with-http-params
;;; probably locale
;;;
(defvar *site-environment*) ;; used in with-site-env

(defun config-param (key)
  ;; 1. first check *site-environment* (cache)
  ;; 2. query DB (and update cache)
  )

(defun %set-config-param (key value)
  ;; 1. update DB
  ;; 2. update cache
  )

(defsetf config-param %set-config-param)

|#

;;; ABSTRACT5.LISP ends here
