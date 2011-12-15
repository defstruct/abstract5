;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
;;
;; Copyright (C) 2011 Jong-won Choi
;; All rights reserved.
;;
;; Author:  $Author$
;; Version: $Id$
;;
;;;; Commentary:
;;
;;
;;
;;;; Code:

(in-package :abstract5)

(defconstant +login-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(define-mvc login () ;; without :site, all sites share this definition
  :model ((:inherit (user-list))
	  (:slots   (filters))
	  (:methods ((get-page ((user-visited-page-list user-visited-page-list) filters)
			       "return user objects"))))
  :view ((:uri  "/user-visited-page-list") ;; will map test.domain/user-visited-page-list
	 ;; map site-instances/test/components/user-visited-page-list.html
	 ;; or  site-instances/root/components/user-visited-page-list.html (when no :site)
	 (:html "component/user-visited-page-list.html")
	 )
  :controller ((:methods ((run-operation&get-env (mvc operator (user-name :user-name))
			    ;; FIXME: macro need to
			    ;; - add mvc-key in argslist
			    ;; - add with-site-db
			    ;; - add with-site-env (for site relevant vars)
			    ;; - add with-http-params
			    ;;
			    ;; If there is any error, show the error later
			    (let ((user-name-label (if (config-param :user-registration-with-email-address)
						       "Email Address"
						       "Username")))
			      (setf user-name (string-trim-whitespace user-name))



			  (search-users ((controller user-visited-page-list) :page-id :dt-from :dt-to :num-results)
			    ;; send this to view
			    (:user-email-class ""
			     :user-email-href ""
			     :first-name-class ""
			     :first-name-href ""
			     :second-name-class ""
			     :second-name-href ""
			     ...
			     :loop-result
			     ((:user-id ""
			       :user-id-href ""
			       :first-name ""
			       :last-name ""
			       ...)
			      (:user-id ""
			       :user-id-href ""
			       :first-name ""
			       :last-name ""
			       ...)
			      ...)))))))
;;; LOGIN.LISP ends here
