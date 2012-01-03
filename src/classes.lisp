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

(defconstant +classes-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

#.(clsql-sys:locally-enable-sql-reader-syntax)

;;;
;;; Classes in global namespace
;;;

(defparameter *global-class-tables-in-db* ())

(defun init-postgresql ()
  (unless (sequence-exists-p [oid-seq])
    (create-sequence [oid-seq]))
  (create-view-from-classes *global-class-tables-in-db*))

(defun fix-oid-autoincrement (class-name)
  (let ((oid-seq "oid_seq")
	(class (find-class class-name)))
    (flet ((maybe-set-oid-seq (slots)
	     (bind-when (oid-slot (find 'oid slots :key #'slot-definition-name))
	       (setf (clsql-sys::view-class-slot-autoincrement-sequence oid-slot) oid-seq))))
      ;; NB: CLSQL uses two different kind of slots (not sure why)
      (maybe-set-oid-seq (clsql-sys::ordered-class-slots class))
      (maybe-set-oid-seq (clsql-sys::keyslots-for-class class)))))

(defmacro define-persistent-class (name (&rest super-classes) &body body)
 `(prog1
      (def-view-class ,name (,@super-classes)
	,@body)
    (fix-oid-autoincrement ',name)
    (pushnew ',name *global-class-tables-in-db*)))

;;;
;;; OID-MIXIN
;;;
(define-persistent-class oid-mixin ()
  ((oid :reader oid :db-kind :key :type integer :db-constraints (:not-null :auto-increment))))

(defgeneric customize-instance! (object)
  (:method (object) (declare (ignore object))))

(defmethod print-object ((self oid-mixin) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (if (slot-boundp self 'oid)
	       (oid self)
	       "unbound")
	   stream))
  self)

(defmethod shared-initialize :after ((instance oid-mixin)
				     slot-names
				     &rest keys)
  (declare (ignore keys slot-names))
  ;; Persistent class (inherited from oid-mixin) may have join + non 'oid' home-key
  ;; which is 'ownership'.
  ;; If that's the case, set the home-key slot if it is unbound
  ;; and the join slot has value.
  (let ((all-slots (clsql-sys::ordered-class-slots (class-of instance))))
    (dolist (slot all-slots)
      (let ((slot-name (slot-definition-name slot)))
	(bind-when (join-value (and (typep instance 'oid-mixin)
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
		    (oid join-value)))))))))

(defun find-persistent-object (name oid &key refresh)
  (first (select name :where [= [oid] oid] :flatp t :refresh refresh)))

(defun make-db-instance (name &rest args)
  ;; NB: update-records-from-instance return primary key.
  ;; make all instances have oid
  (let ((object (apply #'make-instance name args)))
    (customize-instance! object)
    (with-transaction ()
      (update-records-from-instance object))
    (if (typep object 'oid-mixin)
	(find-persistent-object name (oid object))
	object)))

;;;
;;; SUBDOMAIN
;;;
(define-persistent-class subdomain (oid-mixin)
  ((name :reader subdomain-name :initarg :name :type text :db-kind :base :db-constraints (:not-null :unique))
   (site-oid :accessor site-oid :initarg :site-oid :type integer :db-kind :base)
   (site :accessor subdomain-site :initarg :site :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set t))))

;;;
;;; ADMIN
;;;
(define-persistent-class admin (oid-mixin)
  ((name :accessor admin-name :initarg :name :type text :db-kind :base)
   ;; email, phone, address, etc
   (site-oid :reader site-oid :type integer :db-kind :base)
   (site :accessor admin-site :initarg :site :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set t))))

;;;
;;; SITE
;;;
(define-persistent-class site (oid-mixin)
  ((name	:reader site-name
		:initarg :name
		:type text
		:db-kind :base
		:db-constraints (:not-null :unique))
   ;; FIXME: add more
   (description :accessor site-description
		:initarg :description
		:type text
		:db-kind :base)
   (home-folder :accessor site-home-folder
		:initarg :home-folder
		:type text
		:db-kind :base)
   (db-schema	:accessor site-db-schema
		:type text
		:db-kind :base
		:db-constraints (:not-null))
   (subdomains	:accessor site-subdomains
		:db-kind :join
		:db-info (:join-class subdomain
				     :home-key oid
				     :foreign-key site-oid
				     :retrieval :deferred
				     :set t))
   (admins	:accessor site-admins
		:db-kind :join
		:db-info (:join-class admin
				      :home-key oid
				      :foreign-key site-oid
				      :retrieval :deferred
				      :set t))
   (record-caches :reader site-record-caches
		  :initform (tg:make-weak-hash-table :test #'equal :weakness :value)
		  :db-kind :virtual)))

;;
;; custom caches - patching the existing clsql-sys method
;;
(in-package :clsql-sys)

(defmethod record-caches :around ((db database))
  (if (boundp 'abstract5::*selected-site*)
      (abstract5::site-record-caches abstract5::*selected-site*)
      (call-next-method)))

(in-package :abstract5)

(defmethod customize-instance! ((self site))
  (declare (special *schema-class-tables-in-db*))
  (let* ((site-name (site-name self))
	 (schema (format nil "\"~A\"" site-name)))
    (setf (site-db-schema self) schema
	  (site-home-folder self) (ensure-site-home-folder site-name))
    ;; TODO: copy files into folders
    (with-transaction ()
      (create-schema schema)
      (with-appending-schema (schema)
	;; TODO: create tables
	(create-view-from-classes *schema-class-tables-in-db*)
	;; custom indexes
	(create-index [idx-unique-uri] :on [uri-handler] :attributes '([uri-path] [uri-filename]))
	(create-default-uri-handlers)))))

(defun find-site-from-subdomain-name (name)
  (with-global-context ()
    (find-persistent-object 'site
			  (site-oid (first (select 'subdomain :where [= [name] name] :flatp t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Schema namespace classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *schema-class-tables-in-db* nil)

(defmacro define-schema-class (name (&rest super-classes) &body body)
 `(prog1
      (def-view-class ,name (,@super-classes)
	,@body)
    (fix-oid-autoincrement ',name)
    (pushnew ',name *schema-class-tables-in-db*)))

;;;
;;; URI and handler table
;;;
(define-schema-class uri-handler ()
  ((uri-path		:accessor uri-handler-uri-path
			:initarg :uri-path
			:type text
			:db-kind :base
			:db-constraints (:not-null))
   (uri-filename	:accessor uri-handler-uri-filename
			:initarg :uri-filename
			:initform nil
			:type text
			:db-kind :base)
   (fs-path		:accessor uri-handler-fs-path
			:initarg :fs-path
			:type text
			:db-kind :base
			:db-constraints (:not-null))
   (fs-filename		:accessor uri-handler-fs-filename
			:initarg :fs-filename
			:initform nil
			:type text
			:db-kind :base)
   (name		:accessor uri-handler-name
			:initarg :name
			:type text
			:db-kind :base)))

(defparameter *default-uri-handlers*
  '(("/js/"		"/js/")
    ("/css/"		"/css/")
    ("/images/"		"/images/")
    ("/html/"		"/html/")
    ("/favicon.ico"	"/images/favicon.ico")
    ("/robots.txt"	"/etc/robots.txt")))

(defun create-default-uri-handlers ()
  (loop for (uri fs) in *default-uri-handlers*
     as (uri-path uri-filename) = (split-path&name uri)
     and (fs-path fs-filename) = (split-path&name fs)
     do (cond ((and uri-filename fs-filename)
	       ;; file
	       (make-db-instance 'uri-handler
				 :uri-path uri-path :uri-filename uri-filename
				 :fs-path fs-path   :fs-filename fs-filename
				 :name "ABSTRACT5::STATIC-FILE-HANDLER"))
	      ((and (null uri-filename) (null fs-filename))
	       ;; folder
	       (make-db-instance 'uri-handler
				 :uri-path uri-path
				 :fs-path fs-path
				 :name "ABSTRACT5::STATIC-FOLDER-HANDLER"))
	      (t (error "Programming error - invalid spec!")))))

#|
(init-postgresql)
(main)
(start-sql-recording)
(clsql-sys::enable-sql-reader-syntax)

(setf s1 (make-instance 'site :subdomain "localhost"
			:description "Test site"))
(update-records-from-instance s1)
(select 'site :where [= [subdomain] "localhost"] :flatp t)
|#

#+FIXME
(def-view-class theme ()
  ((thumnail)
   (name)
   (description)
   (header.html)
   (css-files)
   (footer.html)
   (default.html))
  )

#.(clsql-sys:locally-disable-sql-reader-syntax)

;;; CLASSES.LISP ends here
