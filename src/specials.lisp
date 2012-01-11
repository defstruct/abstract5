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

(defconstant +specials-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")


;;;
;;; Errors, conditions
;;;
(define-condition internal-error ()
  ((message :type string :initarg :message :reader internal-error-message)))

(defvar *internal-errors*) ;; used to accumulate error messages
(defun push-internal-error (error-message)
  (push error-message *internal-errors*))
(defun get-internal-errors ()
  (reverse *internal-errors*))

(define-condition site-not-found ()
  ((domain :type string :initarg :domain :reader site-not-found-domain))
  (:report (lambda (condition stream)
              (format stream "Matching site not found: ~S"
                      (site-not-found-domain condition)))))

(defparameter *abstract5-home*
  (namestring (make-pathname :directory (butlast (pathname-directory *load-pathname*)))))

(defparameter *abstract5-sub-folders*
  '("conf" "templates" "js" "css" "images" "html" "etc"
    #+FIXME "packages" #+FIXME "themes")
  "Pathname for the 'abstract5/site-instances/*'")

(defun make-abstract5-folder (dir1 dir2)
  (namestring (make-pathname :directory dir1
			     :name dir2)))

(defparameter *abstract5-home-folders*
  (mapcar (lambda (str)
	    (cons (keyword (string-upcase str))
		  (make-abstract5-folder *abstract5-home* str)))
	  *abstract5-sub-folders*)
  "Pathname for the 'abstract5/site-instances/*'")

(defun abstract5-folder (keyword &key (test #'eql) (key #'identity))
  (cdr (assoc keyword *abstract5-home-folders* :test test :key key)))

(defun get-config-parameters (def keys)
  (mapcar (lambda (key)
	    (getf def key))
	  keys))

(defun ensure-site-sub-folders (site-home)
  (ccl::recursive-copy-directory (merge-pathnames "site-template" *abstract5-home*)
				 site-home))

(defun ensure-site-home-folder (site-name)
  (let ((home (merge-pathnames (make-pathname :directory  (format nil "sites/~A/" site-name))
			       *abstract5-home*)))
    (ensure-site-sub-folders home)
    (namestring home)))


;;; Dynamic variables for site namespace
(defvar *selected-site*)

(defmacro with-site-context ((var domain) &body body)
  `(bind-if (*selected-site* (find-site-from-subdomain-name ,domain))
	    (let ((,var *selected-site*))
	      (on-schema ((site-db-schema *selected-site*))
		,@body))
	    (error 'site-not-found :domain ,domain)))

(defmacro using-public-db-cache (&body body)
  ;; This only works with cached object.
  ;; Because of ON-SCHEMA, non-chached object query will fail.
  `(let ((*selected-site* *selected-site*))
     (makunbound '*selected-site*)
     ,@body))

(defun insert-assertion (fn-body assertion)
  (flet ((doc-or-type-decl? (elem)
	   (or (stringp elem)
	       (and (consp  elem)
		    (eq (first elem) 'declare)))))
    (let ((pos (position-if-not #'doc-or-type-decl? fn-body)))
      (if (zerop pos)
	  (cons assertion fn-body)
	  (let ((assertion-list (list assertion)))
	    (setf (cdr assertion-list) (nthcdr pos fn-body))
	    (setf (nthcdr pos fn-body) assertion-list)
	    fn-body)))))

(defmacro site-function (spec args &body body)
  `(defun ,spec ,args
     ,@(insert-assertion body '(assert *selected-site*))))

(defmacro site-method (spec args &body body)
  `(defmethod ,spec ,args
     ,@(insert-assertion body '(assert *selected-site*))))

(defmacro global-function (spec args &body body)
  `(defun ,spec ,args
     ,@(insert-assertion body '(assert (not (boundp '*selected-site*))))))

(defmacro global-method (spec args &body body)
  `(defmethod ,spec ,args
     ,@(insert-assertion body '(assert (not (boundp '*selected-site*))))))

;;; SPECIALS.LISP ends here
