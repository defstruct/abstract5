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

(defconstant +generic-methods-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

;; CONTROLLER Generic Functions
(defgeneric get-view-environment (controller)
  (:documentation
   "Return an environment of key value pairs as in HTML-TEMPLATE library."))

;; VIEW Generic Functions
(defgeneric render-html (view env)
  (:documentation
   "Render HTML file for the given VIEW using ENV context. ENV is usable key value pairs in HTML-TEMPLATE library."))

;;; GENERIC-METHODS.LISP ends here
