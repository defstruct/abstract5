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
  (clsql:execute-command (format nil "SET search_path TO ~S" name)))

(defun append-search-path (name)
  (clsql:execute-command (format nil "SET search_path TO ~S,~S" name *current-db-schema*)))

(defun create-schema (name)
  (clsql:execute-command (format nil "CREATE SCHEMA ~A" name)))

(defun schema-exists-p (schema-name)
  (and (clsql:query (format nil "select nspname from pg_catalog.pg_namespace where nspname='~A'" schema-name))
       t))

(defun delete-schema (schema-name &key if-exists)
  (clsql:execute-command (format nil "DROP SCHEMA~:[~; IF EXISTS~] ~S CASCADE"
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

(defparameter *public-sql-command-list* nil)

(defun init-public-sql ()
  (unless *public-sql-command-list*
    (setf *public-sql-command-list* (cl-ppcre:split ";\\n\\n"
						    (read-text-file (make-pathname :directory (abstract5-folder :conf)
										   :name "public-schema"
										   :type "sql")))))
  (unless (query "select proname from pg_proc where proname = 'new_schema_and_get_prev_schema'")
    (dolist (command *public-sql-command-list*)
      (clsql-sys:execute-command command))))

(defparameter *schema-sql-command-list* nil)

(defun init-schema-sql ()
  (unless *schema-sql-command-list*
    (setf *schema-sql-command-list* (cl-ppcre:split ";\\n\\n"
						    (read-text-file (make-pathname :directory (abstract5-folder :conf)
										   :name "site-schema"
										   :type "sql")))))
  (dolist (command *schema-sql-command-list*)
    (clsql-sys:execute-command command)))

;;
;; OID-MIXIN
;;

(defclass persistent-class (clsql-sys::standard-db-class)
  ())

;;
;; All persistent object book keeping table
;;
(def-view-class pobject ()
  ((oid :accessor persistent-object-oid :db-kind :key :type integer :db-constraints (:not-null))
   (schema :reader persistent-object-schema :init-arg :schema :db-kind :base :type text)
   (class :reader persistent-object-class :init-arg :class :db-kind :base :type symbol)))

(defvar *persistent-object-caches* (make-hash-table :test #'eq :weak :value))
(def-view-class persistent-object (clsql-sys::standard-db-object)
  ((oid :accessor persistent-object-oid :db-kind :key :type integer :db-constraints (:not-null)))
  (:metaclass persistent-class))

(defmethod save-pobj-to-cache ((pobj persistent-object))
  (setf (gethash (persistent-object-oid pobj) *persistent-object-caches*) pobj))

(defmethod get-pobj-from-cache ((oid integer))
  (gethash oid *persistent-object-caches*))

(defmethod print-object ((self persistent-object) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (if (slot-boundp self 'oid)
	       (persistent-object-oid self)
	       "unbound")
	   stream))
  self)

(defmethod shared-initialize :after ((instance persistent-object)
				     slot-name
				     &rest keys)
  (declare (ignore keys slot-name))
  ;; Persistent class (inherited from oid-mixin) may have join + non 'oid' home-key
  ;; which is 'ownership'.
  ;; If that's the case, set the home-key slot if it is unbound
  ;; and the join slot has value.
  (let ((all-slots (clsql-sys::ordered-class-slots (class-of instance))))
    (dolist (slot all-slots)
      (let ((slot-name (slot-definition-name slot)))
	(bind-when (join-value (and (typep instance 'persistent-object)
				    (eq (clsql-sys::view-class-slot-db-kind slot) :join)
				    (slot-boundp instance slot-name)
				    (slot-value instance slot-name)))
	  (let* ((db-info (clsql-sys::view-class-slot-db-info slot))
		 (join-id-slot-name (gethash :home-key db-info)))
	    ;; when it is ownership...
	    (when (and (not (eq join-id-slot-name 'oid))
		       (not (slot-boundp instance join-id-slot-name) )
		       (eq (gethash :foreign-key db-info) 'oid))
	      (setf (slot-value instance join-id-slot-name)
		    (persistent-object-oid join-value)))))))))

#.(clsql-sys:locally-enable-sql-reader-syntax)

(defun find-persistent-object (oid &key refresh)
  (or (get-pobj-from-cache oid)
      (let ((pobj-record (first (select [oid] [schema] [class] :from [public pobj] :where [= [pobj oid] oid] :flatp t))))
	(when pobj-record
	  (destructuring-bind (oid schema class)
	      pobj-record
	    (on-schema ((format nil "~S" schema))
	      (let ((class-symbol (find-symbol class)))
		(first (select class-symbol
			       :where [= (sql-expression :attribute 'oid :table class-symbol) oid]
			       :flatp t
			       :refresh refresh)))))))))

#.(clsql-sys:locally-disable-sql-reader-syntax)

(defmacro define-persistent-class (name (&rest super-classes) &body body)
 `(prog1
      (def-view-class ,name (persistent-object ,@super-classes)
	,@body
	(:metaclass persistent-class))))

(in-package #:clsql-sys)

;;
;; 1. (view-class-table (view-table view-class)) -> (view-class-table (class-name view-class))
;; 2. No normalization
;; 3. PK slot is 'oid' and no use.
;; 4. no slots with defaults
;;
(defun custom-insert-records (obj slots view-class-table)
  (loop for slot in slots
     as value = (slot-value obj (slot-definition-name slot))
     do (check-slot-type slot value)
     collect (sql-escape (view-class-slot-column slot))
     into attributes
     collect (db-value-from-slot slot value (or (view-database obj) *default-database*))
     into values
     finally (return
	       (abstract5::exec-stored-function :insert_pobj
						(if (boundp 'abstract5::*current-db-schema*)
						    abstract5::*current-db-schema*
						    "public")
						(symbol-name view-class-table)
						;; into
						(sql-escape view-class-table)
						;; attributes
						(format nil " (~{~A, ~} oid) " attributes)
						;; values
						(format nil " (~{'~A', ~}" values)))))

(defmethod update-records-from-instance ((obj abstract5::persistent-object)
                                         &key database this-class)
  (let ((database (or database (view-database obj) *default-database*)))
    (flet ((slot-storedp (slot)
	     (and (member (view-class-slot-db-kind slot) '(:base :key))
		  (slot-boundp obj (slot-definition-name slot)))))
      (let* ((view-class (or this-class (class-of obj)))
             (view-class-table (class-name view-class))
	     (slots (remove-if-not #'slot-storedp (ordered-class-slots view-class))))
	(cond ((view-database obj)
	       (flet ((slot-value-list (slot)
			(let ((value (slot-value obj (slot-definition-name slot))))
			  (check-slot-type slot value)
			  (list (sql-expression :attribute (view-class-slot-column slot))
				(db-value-from-slot slot value database)))))
		 (update-records (sql-expression :table view-class-table)
				 :av-pairs (mapcar #'slot-value-list slots)
				 :where (key-qualifier-for-instance
					 obj :database database
					 :this-class view-class)
				 :database database)))
	      (t
	       (setf (abstract5::persistent-object-oid obj)
		     (custom-insert-records obj slots view-class-table))
	       (when (eql this-class nil)
		 (setf (slot-value obj 'view-database) database))))))
    (abstract5::persistent-object-oid obj)))

;;; DATABASE.LISP ends here
