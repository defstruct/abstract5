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
  :view ((:uri  "/login.html")
	 (:html "html-views/login.html"))
  :controller ((:methods ((do-login (mvc operator
					 (user-name :user-name :string) (password :password :string)
					 (maintain-login :maintain-login :boolean)
					 ;; FIXME: :list vs :ordered-list
					 (attributes :user-attributes :list))
			    (when (banned-ip (hunchentoot:real-remote-addr))
			      (error 'mvc-error :message (get-mvc-message :ip-banned)))
			    ;; FIXME: open-id-auth
			    (when (or (whitespace-string? user-name)
				      (whitespace-string? password))
			      (error 'mvc-error :message (get-mvc-message :user-name-passowrd-required
									  (config-param :user-registration-mode))))
			    ;; FIXME: login-user has optional site arg
			    ;; and with-mvc-site-env *mvc-site* is the default
			    (let ((user (login-user user-name password)))
			      (bind-case (status (user-status user))
				((:user-non-validated :user-inactive)
				 (error 'mvc-error (get-mvc-message status)))
				((:user-invalid)
				 (error 'mvc-error (get-mvc-message status (config-param :user-registration-mode)))))

			      (when maintain-login
				(maintain-login-via-cookie user))

			      ;; FIXME: Does user need locale? Why not site based locale? (in php code)

			      (when attributes
				(fill-unfiled-attributes-FIXME)
				(bind-when (unfilled-attributes (collect-FIXME-unfilled-attributes))
					   (logout-user user)
					   (return (:FIXME))))

			      ;; FIXME: php fire on-user-login here

			      ;; FIXME: json login?
			      ;;header("Cache-Control: no-store, no-cache, must-revalidate");
			      ;;header("Pragma: no-cache");
			      ;;header('Expires: Fri, 30 Oct 1998 14:19:41 GMT'); //in the past

			      (clear-http-cache)
			      (hunchentoot:redirect (redirect-uri (or (user-redirect-uri user) (site-redirect-uri)) "/"))

			      ;; FIXME: macro need to
			      ;; - add mvc-key in argslist
			      ;; - add with-site-db
			      ;; - add with-site-env (for site relevant vars)
			      ;; - add with-http-params
			      ;; - openID stuff

			      ))))))


;; FIXME: move to *root-site* definition file
(define-mvc-messages login () ;; (:site test) , etc
  ((:ip-banned "Unable to complete action: your IP address has been banned. Please contact the administrator of this site for more information.")
   (:user-name-passowrd-required :use-email "An email address and password are required.")
   (:user-name-passowrd-required :use-user-name "A username and password are required.")
   (:user-non-validated "This account has not yet been validated. Please check the email associated with this account and follow the link it contains.")
   (:user-invalid :use-email "Invalid email address or password.")
   (:user-invalid :use-user-name "Invalid username or password.")
   (:user-inactive "This user is inactive. Please contact us regarding this account.")))


;;; LOGIN.LISP ends here
