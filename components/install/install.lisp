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
   Report bugs to: jongwon.choi@internode.on.net")
;; theme, package, resource, blocks...

(define-html-page install ("install") ;; URI->page mapping
  :model	()
    :controller (:before )
    :view	install.html
    :theme	core)

(defun

;;; INSTALL.LISP ends here
