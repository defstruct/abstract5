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

(defgeneric customize-instance! (object)
  (:method (object) (declare (ignore object))))

(defun make-db-instance (name &rest args)
  ;; NB: update-records-from-instance return primary key.
  ;; make all instances have oid
  (let ((object (apply #'make-instance name args)))
    (customize-instance! object)
    (with-transaction ()
      (update-records-from-instance object))
    object))
;;
;; SUBDOMAIN
;;
(define-persistent-class subdomain ()
  ((name :reader subdomain-name :initarg :name :type text :db-kind :base :db-constraints (:not-null :unique))
   (site-oid :accessor site-oid :initarg :site-oid :type integer :db-kind :base)
   (site :accessor subdomain-site :initarg :site :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set nil))))

;;
;; ADMIN
;;
(define-persistent-class admin ()
  ((name :accessor admin-name :initarg :name :type text :db-kind :base
	 :db-constraints (:not-null))
   ;; email, phone, address, etc
   (site-oid :reader site-oid :type integer :db-kind :base)
   (site :accessor admin-site :initarg :site :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set nil))))

;;
;; SITE
;;
(define-persistent-class site ()
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
   (locale	:accessor site-locale
		:initarg :locale
		:initform "en_AU.UTF-8"		; FIXME: someday support other
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
		  :initform (make-hash-table :test #'equal :weak :value)
		  :db-kind :virtual)))

(defmethod site-language ((site site))
  (let* ((locale (site-locale site))
	 (_pos (position #\_ locale :test #'char=)))
    (assert _pos)
    (subseq locale 0 _pos)))

(defun site-language* ()
  (assert *selected-site*)
  (site-language *selected-site*))

(defmethod site-encoding ((site site))
  (let* ((locale (site-locale site))
	 (.pos (position #\. locale :test #'char=)))
    (assert .pos)
    (subseq locale (1+ .pos))))

(site-function site-encoding* ()
  (site-encoding *selected-site*))

;;
;; custom caches - patching the existing clsql-sys method
;;
(in-package :clsql-sys)

(defmethod record-caches :around ((db database))
  (if (boundp 'abstract5::*selected-site*)
      (abstract5::site-record-caches abstract5::*selected-site*)
      (call-next-method)))

(in-package :abstract5)
(defvar *page-entries*)
(defmethod customize-instance! ((self site))
  (declare (special *schema-class-tables-in-db*))
  (let* ((site-name (site-name self))
	 (schema (format nil "\"~A\"" site-name)))
    (setf (site-db-schema self) schema
	  (site-home-folder self) (ensure-site-home-folder site-name))
    ;; TODO: copy files into folders
    (with-transaction ()
      (create-schema schema)
      (on-schema (schema)
	(init-schema-sql)
	(let ((*selected-site* self)
	      (*page-entries* nil))
	  (load (make-pathname :directory (abstract5-folder :src)
			       :name "system-html-page-entries"
			       :type "lisp")))))))

(defun find-site-from-subdomain-name (name)
  (using-public-db-cache
    (find-persistent-object (site-oid (first (select 'subdomain :where [= [name] name] :flatp t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Schema namespace classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; URI and handler table
;;;
(define-persistent-class page-entry ()
  ((name	 :accessor page-entry-name
		 :initarg :name
		 :type text
		 :db-kind :base)
   (description  :accessor page-entry-description
		 :initarg :description
		 :type text
		 :db-kind :base)
   (status	 :accessor page-entry-status
		 :initarg :status
		 :initform :enabled
		 :type keyword
		 :db-kind :base)
   (uri-path	 :accessor page-entry-uri-path
		 :initarg :uri-path
		 :type text
		 :db-kind :base
		 :db-constraints (:not-null))
   (uri-filename :accessor page-entry-uri-filename
		 :initarg :uri-filename
		 :type text
		 :db-kind :base)
   (reader	 :accessor page-entry-reader
		 :initarg :reader
		 :type symbol
		 :db-kind :base)
   (evaluator	 :accessor page-entry-evaluator
		 :initarg :evaluator
		 :type symbol
		 :db-kind :base)
   (printer	 :accessor page-entry-printer
		 :initarg :printer
		 :type symbol
		 :db-kind :base)
   (env		 :accessor page-entry-env
		 :initarg :env
		 :initform nil
		 :type list
		 :db-kind :base)
   (pathname	 :accessor page-entry-pathname
		 :initarg :pathname
		 :type text
		 :db-kind :base)
   (parent-oid   :readder parent-oid :type integer :db-kind :base)
   (parent	 :accessor page-entry-parent :initarg :parent :db-kind :join
		 :db-info (:join-class page-entry
				       :home-key parent-oid
				       :foreign-key oid
				       :retrieval :deferred
				       :set nil))
   (children	 :accessor page-entry-children
		 :db-kind :join
		 :db-info (:join-class page-entry
				       :home-key oid
				       :foreign-key parent-oid
				       :retrieval :deferred
				       :set t))))

(defmethod enabled-p ((page-entry page-entry))
  (eq (page-entry-status page-entry) :enabled))

;;
;; There are two cases to construct SQL expression in URI->SQL-PAGE-ENTRY-WHERE:
;;
;; /a/b/c/d.txt -> ((/a/b/c/ . d.txt) (/a/b/c/ . nil) (/a/b/ . nil) (/a/ . nil) (/ . nil))
;; /a/b/c/      -> ((/a/b/   . c)     (/a/b/c/ . nil) (/a/b/ . nil) (/a/ . nil) (/ . nil))
;;
(defun uri->sql-page-entry-where (uri)
  (destructuring-bind (path filename)
      (split-path&name uri)
    (let ((base-case (if filename
			 [and [= [slot-value 'page-entry 'uri-path] path]
			      [= [slot-value 'page-entry 'uri-filename] filename]]
			 (let ((path-len (length path)))
			   (when (> path-len 1)
			     (destructuring-bind (path2 filename2)
				 (split-path&name (subseq path 0 (1- path-len)))
			       [and [= [slot-value 'page-entry 'uri-path] path2]
			            [= [slot-value 'page-entry 'uri-filename] filename2]]))))))
      (loop for pos in (positions #\/ path :test #'char= :from-end t)
	 collect [and [= [slot-value 'page-entry 'uri-path] (subseq path 0 (1+ pos))]
		      [null [slot-value 'page-entry 'uri-filename]]]
	 into all-cases
	 finally (return (apply #'clsql-sys:sql-operation 'or (cons base-case all-cases)))))))


(site-function find-page-entry (uri)
  (let ((result (select 'page-entry
			  :where (uri->sql-page-entry-where uri)
			  :flatp t
			  :order-by [slot-value 'page-entry 'uri-filename])))
    ;; Because of order-by, first match will be right one.
    (first result)))

;;
;; Block (in concrete5 term)
;;



#|
(init-public-sql)
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
