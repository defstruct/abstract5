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

(defconstant +install-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")
;; theme, package, resource, blocks...

(define-html-page install ("install") ;; URI->page mapping
  :model	()
  :controller	(:before nil :primary %install :after nil)
  :view		(:theme core :header "header" :template "install.html"))

(:html-lang "abc"
 :theme-html-header ((path1 :var "xxx")
		     (path2 :var "yyy"))
 :theme-html-body ((path1 :var "xxx")
		     (path2 :var "yyy"))
 :theme-html-footer ((path1 :var "xxx")
		     (path2 :var "yyy"))
 :theme-css-files ((:css-file "ccm.default.theme.css"))
 ;; no :extra-header
 )

;;; INSTALL.LISP ends here
