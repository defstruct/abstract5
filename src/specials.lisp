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

(defparameter *abstract5-home-dir* (directory-namestring (merge-pathnames "../" *load-pathname*))
  "Pathname for the 'abstract5/site-instances/defstruct'")

(defun get-config-parameters (def keys)
  (mapcar (lambda (key)
	    (getf def key))
	  keys))

(defparameter *site-sub-folders*
  ;; FIXME: not finalised yet
  (mapcar #'(lambda (dir)
	      (make-pathname :directory `(:relative ,dir)))
	  '("packages" "themes")))

(defun ensure-site-sub-folders (site-home)
  (dolist (path *site-sub-folders*)
    (ensure-directories-exist (merge-pathnames path site-home))))

(defun ensure-site-home-folder (site-name)
  (let ((home (merge-pathnames (make-pathname :directory  (format nil "~(~A~)/" site-name))
			       *abstract5-home-dir*)))
    (ensure-site-sub-folders home)
    (namestring home)))

;;; Dynamic variables for site namespace
(defvar *site-database-schema*)
(defvar *site-home-dir*)

(defmacro with-site-context ((domain) &body body)
  (let ((site (gensym "site")))
    `(let ((,site (find-site-from-subdomain-name ,domain)))
       (let ((*site-database-schema* (site-db-schema ,site))
	     (*site-home-dir*	     (site-home-folder ,site)))
	 (progn
	   ,@body)))))

;;; SPECIALS.LISP ends here
