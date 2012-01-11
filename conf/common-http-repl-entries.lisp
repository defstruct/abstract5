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

(defconstant +common-http-repl-entries-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(define-repl-entry ("/robots.txt" "/etc/robots.txt")
  :printer   print-static-file)

(define-repl-entry ("/favicon.ico" "/images/favicon.ico")
  :printer   print-static-file)

(define-repl-entry ("/images/" "/images/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/html/" "/html/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/css/" "/css/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/js/" "/js/")
  :evaluator uri-file->full-pathname
  :printer   print-static-file)

(define-repl-entry ("/core/")
  :reader    read-core-request
  :evaluator eval-core-request
  :printer   print-core-request)

(define-repl-entry ("error/404" "/templates/page-not-found.html")
  :env	     ((:error-code . 404))
  :evaluator get-error-template-and-env
  :printer   print-template-for-error)

;;; COMMON-HTTP-REPL-ENTRIES.LISP ends here
