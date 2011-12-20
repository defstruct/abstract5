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
;;;
;;; Because of concrete5 schema design, it seems really hard to use it as 'object blue print'.
;;; Play with mysql DB first and see if switching to postgresql is feasible.
;;;
;;; I do not want to change existing concrete5, but want piggy backing by just using it.
;;;

#|
(defparameter *lsexp->postgres-mapping*


(defun lsexp->postgresql (def)


  (:TABLE :NAME "AreaGroupBlockTypes"
	  (:FIELD :SIZE "10" :TYPE "I" :NAME "cID" (:KEY) (:DEFAULT :VALUE "0")
		  (:UNSIGNED))
	  (:FIELD :SIZE "255" :TYPE "C" :NAME "arHandle" (:KEY))
	  (:FIELD :SIZE "10" :TYPE "I" :NAME "gID" (:KEY) (:DEFAULT :VALUE "0")
		  (:UNSIGNED))
	  (:FIELD :SIZE "10" :TYPE "I" :NAME "uID" (:KEY) (:DEFAULT :VALUE "0")
		  (:UNSIGNED))
	  (:FIELD :SIZE "10" :TYPE "I" :NAME "btID" (:KEY) (:DEFAULT :VALUE "0")
		  (:UNSIGNED)))


(def-view-class area-group-block-types ()
  (
|#

;;; CLASS-GENERATOR.LISP ends here
