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

;;;
;;; Server start
;;;
(defun start-abstract5 ()
  (let ((hunchentoot:*dispatch-table*
  )

(defun main ()
  ;; check config file. Load it or start installation web page
  ;; DB(?) when?
  ;; check update/migration file. Load and start migration
  ;; check patch files. Load them

  ;; timezone, session,
  )

(defun handler ()
  ;; check maintenance?
  ;; ...
  (multiple-value-bind (model view controller)
      ;; FIXME:
      (get-mvc-from-uri FIXME)
    (render-html view (get-view-environment controller))))

;;; ABSTRACT5.LISP ends here
