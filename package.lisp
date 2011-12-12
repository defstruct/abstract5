;;;;   -*- Mode: lisp; Package: cl-user; Syntax: Common-lisp; encoding: utf-8; -*-
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

(defpackage "ABSTRACT5"
;;  (:nicknames "DEFSTRUCT")
  (:use :cl :utils :hunchentoot :hunchentoot-base :clsql)
  (:import-from :abstract5-asd :+abstract5-version+))

(in-package :defstruct)

(defconstant +package-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")


;;; PACKAGE.LISP ends here
