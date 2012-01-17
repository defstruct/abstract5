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
;; Patched to save and use result with abstract5's persistent objects cache
;; ('abstract5' prefixed)
;;
(defun build-objects (vals sclasses immediate-join-classes sels immediate-joins database refresh flatp instances)
  "Used by find-all to build objects."
  (flet ((build-object (vals vclass jclasses selects immediate-selects instance)
	   (let ((obj (when (and (not refresh) (typep vclass 'abstract5::persistent-class))
			(abstract5::get-pobj-from-cache (nth (position 'abstract5::oid selects
								       :key #'(lambda (sl)
										(slot-definition-name (car sl))))
							     vals)))))

	     (unless obj
	       (setf obj (if instance instance (make-instance (class-name vclass) :view-database database)))
	       (let* ((db-vals (butlast vals (- (list-length vals)
						(list-length selects))))
		      (join-vals (subseq vals (list-length selects)))
		      (joins (mapcar #'(lambda (c) (when c (make-instance c :view-database database)))
				     jclasses)))
		 ;; use refresh keyword here
		 (setf obj (get-slot-values-from-view obj (mapcar #'car selects) db-vals))
		 (mapc #'(lambda (jo)
			   ;; find all immediate-select slots and join-vals for this object
			   (let* ((jo-class (class-of jo))
				  (slots
				   (if (normalizedp jo-class)
				       (class-direct-slots jo-class)
				       (class-slots jo-class)))
				  (pos-list (remove-if #'null
						       (mapcar
							#'(lambda (s)
							    (position s immediate-selects
								      :key #'car
								      :test #'eq))
							slots))))
			     (get-slot-values-from-view jo
							(mapcar #'car
								(mapcar #'(lambda (pos)
									    (nth pos immediate-selects))
									pos-list))
							(mapcar #'(lambda (pos) (nth pos join-vals))
								pos-list))))
		       joins)
		 (mapc
		  #'(lambda (jc)
		      (let* ((vslots
			      (class-slots vclass))
			     (slot (find (class-name (class-of jc)) vslots
					 :key #'(lambda (slot)
						  (when (and (eq :join (view-class-slot-db-kind slot))
							     (eq (slot-definition-name slot)
								 (gethash :join-class (view-class-slot-db-info slot))))
						    (slot-definition-name slot))))))
			(when slot
			  (setf (slot-value obj (slot-definition-name slot)) jc))))
		  joins)
		 (when refresh (instance-refreshed obj))))
	     ;; Save the object
	     (abstract5::save-pobj-to-cache obj)
	     obj)))
    (let* ((objects
            (mapcar #'(lambda (sclass jclass sel immediate-join instance)
                        (prog1
                            (build-object vals sclass jclass sel immediate-join instance)
                          (setf vals (nthcdr (+ (list-length sel) (list-length immediate-join))
                                             vals))))
                    sclasses immediate-join-classes sels immediate-joins instances)))
      (if (and flatp (= (length sclasses) 1))
          (car objects)
          objects))))

;;
;; Use weak-hash for records-cache
;;
(defun (setf records-cache-results) (results targets qualifiers database)
  (unless (record-caches database)
    (setf (record-caches database)
          (make-hash-table :test #'equal :weak :value)))
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

;;
;; Pool (to prevent connection mess up in multi-threads env)
;;

(in-package :clsql-postgresql-socket)

(defconstant +postgres-max-connections+ 50
  "Default value of max_connections in postgresql conf is 100.")
(defvar *postgres-connection-count* 0)
(defparameter *postgres-connection-pool* ())
(defparameter *postgresql-pool-lock* (ccl:make-lock "psql-pool-lock"))

(defun get-conn-from-pool ()
  (or (ccl:with-lock-grabbed (*postgresql-pool-lock*)
	(pop *postgres-connection-pool*))
      ;; FIXME: error handling?
      (unless (ccl:with-lock-grabbed (*postgresql-pool-lock*)
		;; ignore race condition
		(> +postgres-max-connections+ +postgres-max-connections+))
	(destructuring-bind (host database user password port)
	    (connection-spec *default-database*)
	  (prog1
	      (open-postgresql-connection :host host :port port
					  :database database :user user
					  :password password)
	    (ccl:with-lock-grabbed (*postgresql-pool-lock*)
	      (incf *postgres-connection-count*)))))))

(defun push-conn-to-pool (conn)
  (ccl:with-lock-grabbed (*postgresql-pool-lock*)
    (push conn *postgres-connection-pool*)))

(defmethod initialize-instance :after ((postgresql-socket-database postgresql-socket-database) &key)
  (push (slot-value postgresql-socket-database 'connection) *postgres-connection-pool*)
  (slot-makunbound postgresql-socket-database 'connection))

(defmethod database-disconnect ((database postgresql-socket-database))
  (dolist (conn *postgres-connection-pool*)
    (close-postgresql-connection conn))
  t)

(defvar *current-db-connection*)

(defmethod database-connection ((database postgresql-socket-database))
  *current-db-connection*)

(defmethod (setf database-connection) (new-value (database postgresql-socket-database))
  (declare (ignore new-value))
  (error "This is not an expected call in pool environment!"))

(defmacro with-connection-from-pool (&body body)
  `(progn
     (assert (not (boundp '*current-db-connection*)))
     (let ((*current-db-connection* (get-conn-from-pool)))
       (unwind-protect (progn ,@body)
	 ;; FIXME: remove unrecoverable error
	 (push-conn-to-pool *current-db-connection*)))))

;;
;; FIXME: Make Hunchentoot workers reusable
;; DB connection and HTTP connection is 1:1
;;

;;; 3RD-PARTY-PATCH.LISP ends here
