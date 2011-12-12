;;;;   -*- Mode: lisp; Package: cl-user; Syntax: Common-lisp -*-
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

(in-package :cl-user)

(defpackage :abstract5-asd
  (:use :cl :asdf))

(in-package :abstract5-asd)

(defconstant +abstract5-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(defsystem :abstract5
  :name "abstract5"
  :author "Jong-won Choi <jongwon.choi@defstruct.com>"
  :maintainer "Jong-won Choi <jongwon.choi@defstruct.com>"
  :license "Proprietorial"
  :serial t
  :description "FIXME"
  :long-description "FIXME"
  :version +abstract5-version+
  :depends-on (:hunchentoot-base :clsql)
  :components ((:file "package")
               (:file "")))

;;; ABSTRACT5.ASD ends here
