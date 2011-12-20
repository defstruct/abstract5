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

(defconstant +login-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(define-mvc login () ;; without :site, all sites share this definition
  :view ((:uri  "/login")
	 (:html "html-views/login.html"))
  :controller ((:methods ((do-login (mvc operator
					 (user-name :user-name :string)
					 (password :password :string)
					 (maintain-login :maintain-login :boolean)
					 (email :email :string)
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
