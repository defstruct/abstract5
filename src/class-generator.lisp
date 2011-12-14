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
;;	Generate persistent class definitions/SQL schema from concrete5 axmls files.
;;
;;;; Code:

(in-package :abstract5)

(defconstant +class-generator-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@internode.on.net")

(asdf:load-system :s-xml :verbose t)
(use-package :s-xml)

(defun axmls-new-element-hook (name attributes seed)
  (cond ((member name '(table field key unsigned index default notnull autoincrement unique col
			deftimestamp opt fulltext
			;;automatic auto autoincrement primary def
			;;  defdate deftimestamp noquote constraints
			)
		 :test #'string-equal)
	 `(,(intern (string-upcase name) :keyword)
	    ,@(mapcan (lambda (pair)
			`(,(intern (string-upcase (car pair)) :keyword)
			   ,(cdr pair)))
		      attributes)))
	((member name '(schema) :test #'string-equal)
	 nil)
	(t (error "Unknown ~S ~S" name attributes))))

(defun axmls-finish-element-hook (name attributes parent-seed seed)
  (when seed
    (if (string-equal name 'table)
	(push seed parent-seed)
	(append parent-seed (list seed)))))

(defun axmls-text-hook (string seed)
  (unless (whitespace-string? string)
    (list string)))

(defun axmls->lsexp (in)
  "Parse and trace a toplevel XML element from stream in"
  (nreverse (first (s-xml:start-parse-xml in
					  (make-instance 's-xml:xml-parser-state
							 :seed (list)
							 :new-element-hook #'axmls-new-element-hook
							 :finish-element-hook #'axmls-finish-element-hook
							 :text-hook #'axmls-text-hook)))))
;;; CLASS-GENERATOR.LISP ends here
