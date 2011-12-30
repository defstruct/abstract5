;;;;   -*- Mode: lisp; Package: cl-user; Syntax: Common-lisp; encoding: utf-8; -*-
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

(defconstant +3rd-party-patch-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(in-package #:clsql-sys)

;;
;; Use weak-hash for records-cache
;;
(defun (setf records-cache-results) (results targets qualifiers database)
  (unless (record-caches database)
    (setf (record-caches database)
          (tg:make-weak-hash-table :test #'equal :weakness :value)))
  (setf (gethash (compute-records-cache-key targets qualifiers)
                 (record-caches database)) results)
  results)

;;
;; PostgreSql text type
;;
(deftype text ()
  "Postgresql varying string"
  'string)

(export 'text :clsql-sys)

(import 'text :clsql)
(import 'text :clsql-user)

(export 'text :clsql)
(export 'text :clsql-user)

;;; 3RD-PARTY-PATCH.LISP ends here
