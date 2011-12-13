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

(defconstant +macros-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(defmacro define-single-page (name (uri) &body body)
  )

(define-html-page install ("install") ;; URI->page mapping
  :model	()
  :controller	(:before nil :primary %install :after nil)
  :view		(:theme core :header "header" :template "install.html"))


;;; MACROS.LISP ends here
