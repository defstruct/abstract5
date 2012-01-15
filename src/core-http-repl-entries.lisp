;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
;;
;; Copyright (C) 2012 Jong-won Choi
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

(defconstant +core-http-repl-entries-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(define-repl-entry ("/robots.txt" "/etc/robots.txt")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/favicon.ico" "/images/favicon.ico")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/images/" "images/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/html/" "html/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/css/" "css/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/js/" "js/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

;; NOTE: 1. No '/'
;;	 2. html-template search path is: theme, site and global
(define-repl-entry ("error/404" "html-templates/page-not-found.html")
  :env	     ((:error-code . 404))
  :evaluator set-env/get-error-template
  :printer   print-standard-html-template)


;;;;;;;;;;;;;
(define-repl-entry ("/dashboard" "html-templates/html-template.html")
    :name "Dashboard"
    :evaluator eval-dashboard-request
    :printer   print-standard-html-template)

(define-repl-entry ("/dashboard/composer" "html-templates/html-template.html")
    :name "Composer Beta"
    :description "Write for your site."
    :parent "/dashboard"
    :evaluator eval-dashboard-request
    :printer   print-standard-html-template)

(define-repl-entry ("/dashboard/sitemap" "html-templates/html-template.html")
    :name "Sitemap"
    :description "Whole world at a glance."
    :parent "/dashboard"
    :evaluator eval-dashboard-request
    :printer   print-standard-html-template)

;; This is an example
(define-repl-entry ("/help")
  :env ((:uri . "http://www.concrete5.org/tools/help_overlay/"))
  :evaluator http-request-using-uri
  :printer   identity)



;;; CORE-HTTP-REPL-ENTRIES.LISP ends here
