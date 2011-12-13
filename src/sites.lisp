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

(defconstant +sites-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(def-abstract5-site root ("www.defstruct" "defstruct")
  "/")

(def-abstract5-site test ("test.defstruct")
  "/test/")

;;; SITES.LISP ends here
