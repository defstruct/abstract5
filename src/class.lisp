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

(defconstant +class-version+ "$Revision$"
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

  (dolist (class *global-class-tables-in-db*)
    (unless (table-exists-p class)
      (create-view-from-class class))))

(defun fix-oid-autoincrement (class-name)
  (let ((oid-seq "oid_seq")
	(class (find-class class-name)))
    (flet ((maybe-set-oid-seq (slots)
	     (bind-when (oid-slot (find 'oid slots :key #'ccl:slot-definition-name))
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

(define-persistent-class oid-mixin ()
  ((oid :reader oid :db-kind :key :type integer :db-constraints (:not-null :auto-increment))))

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
      (let ((slot-name (ccl:slot-definition-name slot)))
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

(define-persistent-class subdomain (oid-mixin)
  ((name :reader site-name :initarg :name :type text :db-kind :base :db-constraints (:not-null :unique))
   (site-oid :accessor site-oid :initarg :site-oid :type integer :db-kind :base)
   (site :accessor subdomain-site :initarg :site :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set t))))

(define-persistent-class admin (oid-mixin)
  ((name :accessor admin-name :initarg :name :type text :db-kind :base)
   ;; email, phone, address, etc
   (site-oid :reader site-oid :type integer :db-kind :base)
   (site :accessor subdomain-site :initarg :site :db-kind :join
	 :db-info (:join-class site
			       :home-key site-oid
			       :foreign-key oid
			       :retrieval :deferred
			       :set t))))

(define-persistent-class site (oid-mixin)
  ((name :reader site-name :initarg :name :type text :db-kind :base :db-constraints (:not-null :unique))
   (description :accessor site-description :initarg :description :type text :db-kind :base)
   (subdomains :accessor site-subdomains :db-kind :join
	       :db-info (:join-class subdomain
				     :home-key oid
				     :foreign-key site-oid
				     :retrieval :deferred
				     :set t))
   (admins :accessor site-admins :db-kind :join
	   :db-info (:join-class admin
				 :home-key oid
				 :foreign-key site-oid
				 :retrieval :deferred
				 :set t))))

(defun find-persistent-object (name oid &key refresh)
  (first (select name :where [= [oid] oid] :flatp t :refresh refresh)))

(defun make-db-instance (name &rest args)
  ;; NB: update-records-from-instance return primary key.
  ;; make all instances have oid
  (let ((object (apply #'make-instance name args)))
    (with-transaction ()
      (update-records-from-instance object))
    (find-persistent-object name (oid object))))

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

;;; CLASS.LISP ends here
