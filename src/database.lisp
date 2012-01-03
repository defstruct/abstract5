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

(defconstant +database-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(defvar *current-db-schema*)
(defmacro on-schema ((schema) &body body)
  `(let ((*current-db-schema* (exec-stored-function :new_schema_and_get_prev_schema ,schema)))
     (unwind-protect (progn ,@body)
       (set-search-path *current-db-schema*))))

(defmacro with-appending-schema ((schema) &body body)
  `(let ((*current-db-schema* (exec-stored-function :new_schema_and_get_prev_schema ,schema)))
     (unwind-protect (progn ,@body)
       (set-search-path *current-db-schema*))))

(defun current-db-schema ()
  (first (clsql:query "select current_schema()" :flatp t)))

(defun set-search-path (name)
  (clsql:execute-command (format nil "SET search_path TO ~A" name)))

(defun append-search-path (name)
  (clsql:execute-command (format nil "SET search_path TO ~A,~A" name *current-db-schema*)))

(defun create-schema (name)
  (clsql:execute-command (format nil "CREATE SCHEMA ~A" name)))

(defun schema-exists-p (schema-name)
  (and (clsql:query (format nil "select nspname from pg_catalog.pg_namespace where nspname='~A'" schema-name))
       t))

(defun delete-schema (schema-name &key if-exists)
  (clsql:execute-command (format nil "DROP SCHEMA~:[~; IF EXISTS~] ~A CASCADE"
			       if-exists
                               schema-name)))

;;;

(defun create-view-from-classes (class-list)
  (dolist (class class-list)
    (unless (table-exists-p class)
      (create-view-from-class class))))

(defun exec-stored-function (fn-name &rest args)
  (caar
   (clsql:query (format nil "select ~A(~{~A~^, ~})"
			fn-name (mapcar #'clsql:sql args)))))


(defun create-stored-procedures (list)
  ;; FIXME: check existence
  (dolist (stored-procedure list)
    (clsql:execute-command stored-procedure)))

(defparameter *common-stored-procedures*
  '("CREATE OR REPLACE FUNCTION new_schema_and_get_prev_schema(TEXT)
RETURNS TEXT
AS $$
   DECLARE
      the_new_schema alias for $1;
      the_old_schema TEXT;
BEGIN
	select current_schema() into the_old_schema;
	execute	'set search_path to ' || the_new_schema;
        RETURN the_old_schema;
END;$$
LANGUAGE plpgsql;"))

;;; DATABASE.LISP ends here
