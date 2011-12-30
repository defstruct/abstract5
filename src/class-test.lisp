;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
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

(in-package :abstract5)

(defconstant +class-test-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(let* ((site (make-db-instance 'site :name "Test localhost" :description "test site"))
       (subdomain (make-db-instance 'subdomain :name "localhost" :site site))
       (admin (make-db-instance 'admin :name "Jong-won Choi" :site site)))
  (list site subdomain admin))
;;(trace (:method iNITIALIZE-INSTANCE :AROUND (CLSQL-SYS::VIEW-CLASS-DIRECT-SLOT-DEFINITION)))
#|
drop table admin;
drop table oid_mixin ;
drop table site      ;
drop table subdomain ;
drop sequence admin_oid_seq    ;
drop sequence oid_seq    ;
drop sequence oid_mixin_oid_seq;
drop sequence site_oid_seq     ;
drop sequence subdomain_oid_seq;
|#
;;; CLASS-TEST.LISP ends here
