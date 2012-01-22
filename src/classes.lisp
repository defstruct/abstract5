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
   (site :accessor admin-site
	 :initarg :site
	 :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set nil))))

;;
;; SITE
;;
(define-persistent-class site ()
  ((orgnasation-oid :accessor site-orgnasation-oid
		    :type integer
		    :db-kind :base)
   (orgnasation :accessor site-orgnasation
		:db-kind :join
		:db-info (:join-class organisation
			  :home-key organisation-oid
			  :foreign-key oid
			  :retrieval :deferred
			  :set nil))
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
   (parent-oid   :reader parent-oid :type integer :db-kind :base)
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
				       :set t))
   (area-template :accessor page-entry-area-template
		  :initarg :area-template
		  :type text
		  :db-kind :base)
   (area-template-key :accessor page-entry-area-template-key
		      :initarg :area-template-key
		      :type keyword
		      :db-kind :base)
   (blocks	 :accessor page-entry-blocks
		 :db-kind :join
		 :db-info (:join-class block
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
(define-persistent-class block ()
  ((id		:accessor block-id
		:initarg :id
		:type keyword
		:db-kind :base)
   (name	 :accessor block-name
		 :initarg :name
		 :type text
		 :db-kind :base)
   (description  :accessor block-description
		 :initarg :description
		 :type text
		 :db-kind :base)
   (content	 :accessor block-content
		 :initarg :content
		 :type text
		 :db-kind :base)
   ;; context+evaluator => pathname, plan HTML, etc
   (evaluator :accessor block-evaluator
	      :initarg :evaluator
	      :type symbol
	      :db-kind :base)
   (parent-oid   :reader parent-oid
		 :initarg :parent-oid
		 :type integer
		 :db-kind :base)
   (parent	 :accessor block-parent
		 :initarg :parent
		 :db-kind :join
		 :db-info (:join-class page-entry
				       :home-key parent-oid
				       :foreign-key oid
				       :retrieval :deferred
				       :set nil))))
;;
;; User
;;

;;
;; NOTE: usage of def-view-class vs define-persistent-class
;;
;;	def-view-class intented not for instantiation
;;	they will be used in define-persistent-class as join classes
;;
(def-view-class address ()
  ((addressable-oid	:initarg :addressable-oid
			:type integer
			:db-kind :base)
   (stereet-number	:accessor address-street-number
			:initarg :street-number
			:type text
			:db-kind :base)
   (street-name		:accessor address-street-name
			:initarg :street-name
			:type text
			:db-kind :base)
   (suburb		:accessor address-suburb
			:initarg :suburb
			:type text
			:db-kind :base)
   (state		:accessor address-state
			:initarg :state
			:type text
			:db-kind :base)
   (post-code		:accessor address-post-code
			:initarg :post-code
			:type text
			:db-kind :base)
   (country		:accessor address-country
			:initarg :country
			:type text
			:db-kind :base)))

(def-view-class phone ()
  ((callable-oid	:initarg :callable-oid
			:type integer
			:db-kind :base)
   ;; type -> phone, fax, mobile
   (type		:accessor phone-type
			:initarg :phone-type
			:type keyword
			:db-kind :base)
   (number		:accessor phone-number
			:initarg :phone-number
			:type text
			:db-kind :base)))

(define-persistent-class organisation ()
  ;; NOTE: organisable-oid object (e.g. site)
  ;; has a salt to encrypt all the information
  ((name	:accessor organisation-name
		:initarg :name
		:type text
		:db-kind :base
		:db-constraints (:not-null))
   (description	:accessor organisation-description
		:initarg :description
		:type text
		:db-kind :base)
   (addresses	:accessor organisation-addresses
		:db-kind :join
		:db-info (:join-class address
			   :home-key oid
			   :foreign-key addressable-oid
			   :retrieval :deferred
			   :set t))
   (phones	:accessor organisation-phones
		:db-kind :join
		:db-info (:join-class phone
			   :home-key oid
			   :foreign-key callable-oid
			   :retrieval :deferred
			   :set t))
   (primary-address-oid	:accessor organisation-primary-address-oid
			:initarg :primary-address-oid
			:type integer
			:db-kind :base)
   (primary-address	:accessor organisation-primary-address
			:db-kind :join
			:db-info (:join-class address
				  :home-key primary-address-oid
				  :foreign-key addressable-oid
				  :retrieval :deferred
				  :set nil))
   (primary-phone-oid	:accessor organisation-primary-phone-oid
			:initarg :primary-phone-oid
			:type integer
			:db-kind :base)
   (primary-phone	:accessor organisation-primary-phone
			:db-kind :join
			:db-info (:join-class phone
				  :home-key primary-phone-oid
				  :foreign-key callable-oid
				  :retrieval :deferred
				  :set nil))))

(define-persistent-class person ()
  ;; NOTE: user-oid object (e.g. user)
  ;; has a salt to encrypt all the information
  ((name	:accessor person-name
		:initarg :name
		:type text
		:db-kind :base
		:db-constraints (:not-null))
   (description	:accessor person-description
		:initarg :description
		:type text
		:db-kind :base)
   (organisation-oid :initarg :organisation-oid
		     :type integer
		     :db-kind :base)
   (organisation :accessor person-organisation
		 :db-kind :join
		 :db-info (:join-class organisation
			   :home-key organisable-oid
			   :foreign-key oid
			   :retrieval :deferred
			   :set nil))
   (addresses	:accessor person-addresses
		:db-kind :join
		:db-info (:join-class address
			   :home-key oid
			   :foreign-key addressable-oid
			   :retrieval :deferred
			   :set t))
   (phones	:accessor person-phones
		:db-kind :join
		:db-info (:join-class phone
			   :home-key oid
			   :foreign-key callable-oid
			   :retrieval :deferred
			   :set t))
   (primary-address-oid	:accessor person-primary-address-oid
			:initarg :primary-address-oid
			:type integer
			:db-kind :base)
   (primary-address	:accessor person-primary-address
			:db-kind :join
			:db-info (:join-class address
				  :home-key primary-address-oid
				  :foreign-key addressable-oid
				  :retrieval :deferred
				  :set nil))
   (primary-phone-oid	:accessor person-primary-phone-oid
			:initarg :primary-phone-oid
			:type integer
			:db-kind :base)
   (primary-phone	:accessor person-primary-phone
			:db-kind :join
			:db-info (:join-class phone
				  :home-key primary-phone-oid
				  :foreign-key callable-oid
				  :retrieval :deferred
				  :set nil))))

(define-persistent-class user ()
  ((name	:accessor user-name
		:initarg :name
		:type text
		:db-kind :base
		:db-constraints (:not-null))
   (truename	:accessor user-true-name
		:initarg :true-name
		:type text
		:db-kind :base)
   (email	:accessor user-email
		:initarg :email
		:type text
		:db-kind :base
		:db-constraints (:not-null))
   (password	:accessor user-password
		:initarg :password
		:type text
		:db-kind :base
		:db-constraints (:not-null))
   (salt :accessor user-salt		;; this will be used encrypt all the user info
		  :initarg :password-salt
		  :type text
		  :db-kind :base
		  :db-constraints (:not-null))
   ;; not sure but possible list is:
   (active-p	:accessor user-active-p
		;; Use DB default instead of :initform nil
		:type boolean
		:db-kind :base
		:db-constraints (:not-null))
   (validated-p	:accessor user-validated-p
		;; Use DB default instead of :initform nil
		:type boolean
		:db-kind :base
		:db-constraints (:not-null))
   #+FIXME
   (full-record-p :accessor user-full-record-p
		:initform nil
		:type boolean
		:db-kind :base
		:db-constraints (:not-null))
   (join-date	:accessor user-join-date
		:initform (get-universal-time)
		:type integer
		:db-kind :base
		:db-constraints (:not-null))
   (avatar-p	:accessor user-avatar-p
		;; Use DB default instead of :initform nil
		:type boolean
		:db-kind :base
		:db-constraints (:not-null))
   (last-online	:accessor user-last-online
		:type integer
		:db-kind :base)
   (last-login	:accessor user-last-login
		:type integer
		:db-kind :base)
   (prev-login	:accessor user-prev-login
		:type integer
		:db-kind :base)
   (num-logins   :accessor user-num-logins
		 :type integer
		 :db-kind :base)
   #+FIXME
   (timezone	:accessor user-timezone
		:type text
		:db-kind :base)
   #+FIXME
   (language :accessor user-language
	     :type text
	     :db-kind :base)
   (person-oid	:accessor user-person-oid
		:type integer
		:db-kind :base)
   (person	:accessor user-person
		:db-kind :join
		:db-info (:join-class person
			  :home-key person-oid
			  :foreign-key oid
			  :retrieval :deferred
			  :set nil))))

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
