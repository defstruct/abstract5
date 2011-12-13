;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
;;
;; Copyright (C) 2011 Jong-won Choi
;; All rights reserved.
;;
;; Author:  $Author$
;; Version: $Id$
;;
;;;; Commentary:
;;
;;
;;
;;;; Code:

(in-package :abstract5)

(defconstant +abstract5-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(defparameter *abstract5-home-dir* (merge-pathnames "../" *load-pathname*)
  "Pathname for the 'abstract5/site-instances/defstruct'")

;;;
;;; Multi-site support
;;;
(defparameter *multi-site-instances* ()
  "List of all multi-site instances. Used in dispatching reqeust, selecting DB schema, choosing site home, etc")

(defstruct abstract5-site
  (:name	nil	:type symbol)
  (:domains	()	:type list)
  (:db-schema	""	:type string)
  (:home	#P""	:type pathname))

(defun get-config-parameters (def keys)
  (mapcar (lambda (key)
	    (getf def key))
	  keys))

(defparameter *site-paths*
  ;; FIXME: not finalised yet
  (mapcar #'make-pathname '("packages" "themes")))

(defun ensure-site-directories-exist (site-home)
  (dolist (path *site-paths*)
    (ensure-directories-exist (merge-pathnames path site-home))))

(defun make-site-instance (site-def)
  (destructuring-bind (name domains)
      (get-config-parameters site-def '(:name :domains))
    (assert (symbolp name) () "Given name ~S is not a symbol" name)
    (assert (and (listp domains) (every #'stringp domains))
	    ()
	    "Given domains ~S must be list of strings" domains)
    (assert (not (find name *multi-site-instances* :key abstract5-site-name))
	    ()
	    "Site ~S exists" name)

    ;; qouted schema name to avoid invalid DB names
    (let ((db-schema (format nil "\"~A\"" name))
	  (home (merge-pathnames (make-pathname :directory  (format nil "~(~A~)/" name))
				   *abstract5-home-dir*)))
      (ensure-site-directories-exist home)

      (make-abstract5-site :name name :domains domains :db-schema db-schema :home home))))

(defun build-multi-site-instances (site-defs)
  (setf *multi-site-instances* (mapcar #'make-site-instance site-defs)))

;;; Dynamic variables for site namespace
(defvar *site-database-schema*)
(defvar *site-home-dir*)

(defmacro with-site-context ((domain) &body body)
  (let ((site (gensym "site")))
    `(let ((,site (find ,domain *multi-site-instances*
			:key #'abstract5-site-domains
			:test (curry-right #'member :test #'string=))))
       (let ((*site-database-schema* (abstract5-site-db-schema ,site))
	     (*site-home-dir*	     (abstract5-site-home ,site)))
	 (progn
	   ,@body)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun http-request-handler (request)
  (with-site-context ((hunchentoot:host request))
    ;; check maintenance?
    ;; ...
    (multiple-value-bind (model view controller)
	;; FIXME:
	(get-mvc-from-uri FIXME)
      (render-html view (get-view-environment controller)))))

(defun main ()
  ;; check config file. Load it or start installation web page
  ;; DB(?) when?
  ;; check update/migration file. Load and start migration
  ;; check patch files. Load them

  ;; timezone, session,
  )


;;;
;;; Replace Hunchentoot's dispatcher function
;;;
(in-package :hunchentoot)

(defun list-request-dispatcher (request)
  (declare (ignore request))
  #'abstract5::http-request-handler)

;;; ABSTRACT5.LISP ends here
