;;;;   -*- Mode: lisp; Package: cl-user; Syntax: Common-lisp -*-
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

(in-package :cl-user)

(defpackage :abstract5-asd
  (:use :cl :asdf))

(in-package :abstract5-asd)

(defconstant +abstract5-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(defsystem :abstract5
  :name "abstract5"
  :author "Jong-won Choi <jongwon.choi@defstruct.com>"
  :maintainer "Jong-won Choi <jongwon.choi@defstruct.com>"
  :license "BSD-style - http://www.opensource.org/licenses/bsd-license.php"
  :serial t
  :description "Concrete5 like CMS with better quality"
;;  :long-description ""
  :version +abstract5-version+
  :depends-on (:hunchentoot :drakma :clsql :clsql-postgresql-socket :html-template)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
	       (:file "3rd-party-patch")
	       (:file "localization")
	       (:file "database")
	       (:file "specials")
	       (:file "classes")
	       (:file "html-page-core")
	       (:file "abstract5")))

;;; ABSTRACT5.ASD ends here
