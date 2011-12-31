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

;;;
;;; MVC functions/generic functions
;;;
;; FIXME: with-mvc-site-env bind *mvc-errors*

(define-condition mvc-error ()
  ((message :type string :initarg :message :initform "" :reader mvc-error-message)))

(defvar *mvc-errors*) ;; used to accumulate error messages
(defun push-mvc-error (error-message)
  (push error-message *mvc-errors*))
(defun get-mvc-errors ()
  (reverse *mvc-errors*))

;; FIXME: with-mvc-site-env bind *mvc-site*
(defvar *mvc-site*)

(defun mvc-message-map (arg)
  (declare (ignore arg))
  "FIXME")

(defun get-mvc-message (&rest keys)
  "Get the message with given keys under *mvc-site*"
  (let ((message (or (gethash (mvc-message-map *mvc-site*) keys)

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
  `(:html-lang ,(get-site-html-lang)))

(defmacro with-mvc-site-env ((mvc) &body body)
  `(let ((*database* (switch-db-schema "FIXME"))
	 (*mvc-errors* ())
	 (*mvc-site* (find-mvc-site??? "FIXME")))
     ,@body))

;;;;

(defgeneric run-operation&get-env (mvc-key mvc operator operands)
  (:documentation
   "Run OPERATOR with OPERANDS of given MVC-KEY and MVC then return an environment of key value pairs as in HTML-TEMPLATE library.")
  (:method append ((key t) mvc operator operands)
	   (declare (ignore mvc))
	   (append (apply operator operands) ))
  (:method-combination append))

(defgeneric render-view-with-env (mvc-key mvc env)
  (:documentation
   "Return HTML view of MVC after applying ENV. MVC-KEY is the method specializer")
  (:method (mvc-key mvc env)
    (declare (ignore mvc-key))
    (html-template:fill-and-print-template (view-template-printer mvc) env :stream *html-output-stream*)))

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



;; FIXME: define VIEW-TEMPLATE-PRINTER



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-abstract5-uri (domain-name uri)
  )

(defun request->subdomain (http-request)
  (let ((host-name (hunchentoot:host http-request)))
    (subseq host-name 0 (or (position #\. host-name)
			    (position #\: host-name)))))

(defun http-request-handler (request)
  (print `(remote-addr ,(hunchentoot:remote-addr request)
		       remote-port ,(hunchentoot:remote-port request)
		       request-method ,(hunchentoot:request-method request)
		       request-uri ,(hunchentoot:request-uri request)
		       server-protocol ,(hunchentoot:server-protocol request)
		       script-name ,(hunchentoot:script-name request)
		       real-remote-addr ,(hunchentoot:real-remote-addr request)
		       user-agent ,(hunchentoot:user-agent )
		       ))
  (with-site-context ((request->subdomain request))
    ;;(print `(,*site-database-schema* ,*site-home-dir*))
    ;; check maintenance?
    ;; ...
    (let ((domain-name (hunchentoot:host request)))
      (multiple-value-bind (mvc operation operands)
	  ;; FIXME: define 404 mvc, Error mvc, etc
	  ;; At this point, mvc and operation have proper values.
	  ;; E.g., mvc can be 404 mvc for invalid operation
	  (parse-abstract5-uri domain-name (hunchentoot:request-uri request))
	(let ((mvc-key (mvc-key mvc)))
	  ;; render-view-with-env may have :before, :after, and :around
	  (with-mvc-site-env (mvc)
	    (let ((mvc-env (handler-bind ((mvc-error #'(lambda (error)
							 (push-mvc-error (error-message error))
							 (throw 'mvc-error-catch nil))))
			     (catch 'mvc-error-catch
			       (run-operation&get-env mvc-key mvc operator operands)))))
	      (set-content-type-header mvc) ;; instead of <meta ... content-type...>
	      (render-view-with-env mvc-key
				    mvc
				    (append mvc-env
					    (get-site-environment)
					    (get-global-environment))))))))))

(defun main ()
  ;; check config file. Load it or start installation web page
  ;; DB(?) when?
  ;; check update/migration file. Load and start migration
  ;; check patch files. Load them

  ;; timezone, session,
  ;; FIXME: read from config file
  (clsql-sys:connect '(#P"/var/run/postgresql/.s.PGSQL.5432" "abstract5" "jc" nil 5432) :encoding :utf-8)

  (setf hunchentoot:*hunchentoot-default-external-format* hunchentoot::+utf-8+)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080)))

;;;
;;; Replace Hunchentoot's dispatcher function
;;;
(in-package :hunchentoot)

(defun list-request-dispatcher (request)
  (abstract5::http-request-handler request))


;;; ABSTRACT5.LISP ends here
