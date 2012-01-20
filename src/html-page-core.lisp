;;;;   -*- Mode: lisp; Package: abstract5; Syntax: Common-lisp; encoding: utf-8; -*-
;;
;; Copyright (C) 2012 Jong-won Choi
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

(defconstant +html-page-core+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(defun find-page-entry-parent (parent)
  (destructuring-bind (path file)
      (split-path&name parent)
    (find-if #'(lambda (entry)
		 (and (string= (page-entry-uri-path entry) path)
		      (string= (page-entry-uri-filename entry) file)))
	     *page-entries*)))
#+XXX
(defmacro define-page-entry ((uri &optional pathname) &key (if-exists :error) env reader evaluator printer
			     name description parent)
  (let ((page-entry (gensym "PAGE-ENTRY")))
    (destructuring-bind (path filename)
	(split-path&name uri)
      `(progn
	 (assert (boundp '*selected-site*))
	 (bind-if (,page-entry (find-page-entry ,uri))
		  (cond ((eq ,if-exists :overwrite)
			 (setf (page-entry-reader ,page-entry) ',reader
			       (page-entry-evaluator ,page-entry) ',evaluator
			       (page-entry-printer ,page-entry) ',printer
			       (page-entry-name ,page-entry) ,name
			       (page-entry-description ,page-entry) ,description)
			 ,@(when parent
				 (setf env (page-entry-env (find-page-entry-parent parent)))
				 `((setf (page-entry-parent ,page-entry)
					 (find-page-entry ,parent))))
			 ,@(when env
				 `((setf (page-entry-env ,page-entry) ',env)))
			 ,@(when pathname
				 `((setf (page-entry-pathname ,page-entry) ,pathname)))
			 (update-records-from-instance ,page-entry)
			 ,page-entry)
			((eq ,if-exists :error)
			 (error "FIXME"))
			(t ,page-entry))
		  (push (make-db-instance 'page-entry
					  :uri-path ,path
					  ,@(when filename
						  `(:uri-filename ,filename))

					  :reader ',reader
					  :evaluator ',evaluator
					  :printer ',printer
					  ,@(when pathname
						  `(:pathname ,pathname))
					  ,@(when env
						  `(:env ',env))
					  ,@(when name
						  `(:name ,name))
					  ,@(when description
						  `(:description ,description))
					  ,@(when parent
						  `(:parent ,(find-page-entry parent))))
			*page-entries*))))))

(defmacro define-page-entry ((uri &optional pathname) &key env reader evaluator printer
			     name description parent area-template area-template-key content-blocks)
  (let ((page-entry (gensym "PAGE-ENTRY"))
	(page-entry-oid (gensym "PAGE-ENTRY-OID")))
    (destructuring-bind (path filename)
	(split-path&name uri)
      `(let (,page-entry)
	 (assert (boundp '*selected-site*))
	 (when (find-page-entry ,uri)
	   (error "Page entry already exists: ~A " uri))
	 (setf ,page-entry (make-db-instance 'page-entry
					     :uri-path ,path
					     ,@(when filename
						     `(:uri-filename ,filename))

					     :reader ',reader
					     :evaluator ',evaluator
					     :printer ',printer
					     ,@(when pathname
						     `(:pathname ,pathname))
					     ,@(when env
						     `(:env ',env))
					     ,@(when name
						     `(:name ,name))
					     ,@(when description
						     `(:description ,description))
					     ,@(when parent
						     (bind-when (parent (find parent *page-entries*
									      :key #'page-entry-uri-path
									      :test #'string=))
						       `(:parent ,parent
								 ,@(when pathname
									 `(:uri-filename ,(page-entry-uri-filename parent) ,parent)))))
					     ,@(when area-template
						     (assert area-template-key)
						     `(:area-template ,area-template))

					     ,@(when area-template-key
						     (assert area-template)
						     `(:area-template-key ,area-template-key))))

	 (let ((,page-entry-oid (persistent-object-oid ,page-entry)))
	   (map nil (lambda (def)
		      (apply #'make-db-instance 'block :parent-oid ,page-entry-oid def))
		',content-blocks)
	   (push ,page-entry *page-entries*))))))

(defmacro with-existing-abstract5-file ((var filename) &body body)
  `(let ((,var (search-file-in-abstract5-system ,filename)))
     ,@body))

(defvar *page-env*)

(site-function append-env-value! (key val)
  (if *page-env*
      (bind-if (found (assoc key *page-env*))
	       (setf (cdr (last val)) (cdr found)
		     (cdr found) val)
	       (push (cons key val) *page-env*))
      (setf *page-env* (list (cons key val)))))

(site-function get-env-value (key)
  (cdr (assoc key *page-env*)))

(site-function set-html-template-value (key val)
  (append-env-value! :html-template `(,key ,val)))

(defvar *current-page-entry*)

(site-function apply-blocks-to-area (page-entry-blocks page-entry-area-template)
  (loop for block in page-entry-blocks
     as key-string = (symbol-name (block-id block))
     append (list (intern (format nil "~A-NAME" key-string) :keyword)
		  (block-name block)
		  (intern (format nil "~A-CONTENT" key-string) :keyword)
		  (funcall (block-evaluator block) (block-content block)))

     into env
     finally (return (with-output-to-string (out)
		       (with-existing-abstract5-file (pathname page-entry-area-template)
			 (let ((html-template:*string-modifier* #'identity))
			   (html-template:fill-and-print-template pathname
								  env
								  :stream out)))))))

(site-function http-read-eval-print-loop (&optional (uri (hunchentoot:script-name*)))
  ;; FIXME add (when (maintenance-p) (find-mvc-entry "error/504"))??
  (let ((*current-page-entry* (find-page-entry uri))
	*page-env*)
    (when (or (null *current-page-entry*) (not (enabled-p *current-page-entry*)))
      (setf *current-page-entry* (find-page-entry "error/404")))
    (with-accessors ((page-entry-env page-entry-env)
		     (page-entry-uri-path page-entry-uri-path)
		     (page-entry-uri-filename page-entry-uri-filename)
		     (page-entry-pathname page-entry-pathname)
		     (page-entry-reader page-entry-reader)
		     (page-entry-evaluator page-entry-evaluator)
		     (page-entry-printer page-entry-printer)
		     (page-entry-area-template page-entry-area-template)
		     (page-entry-area-template-key page-entry-area-template-key)
		     (page-entry-blocks page-entry-blocks))
	*current-page-entry*
      (setf *page-env* page-entry-env)
      ;; Common env
      (append-env-value! :html-template `(:html-lang ,(site-language*)
						 :charset ,(site-encoding*)
						 :title ,(format nil "~A :: ~A"
								 (site-name *selected-site*)
								 (page-entry-name *current-page-entry*))))
      #+XXX
      (print `(page-entry-env ,page-entry-env
			      page-entry-pathname ,page-entry-pathname
			      page-entry-reader ,page-entry-reader
			      page-entry-evaluator ,page-entry-evaluator
			      page-entry-printer ,page-entry-printer))
      (let ((eval-args (multiple-value-list (cond ((and page-entry-uri-path page-entry-uri-filename)
						   ;; file mapping
						   page-entry-pathname)
						  (page-entry-uri-path
						   ;; dir mapping
						   (format nil "~A~A"
							   page-entry-pathname
							   (subseq (script-name*) (length page-entry-uri-path))))
						  (t (funcall page-entry-reader))))))
	#+XXX
	(print `(eval-args ,eval-args ,page-entry-pathname ,(format nil "~A~A"
								    page-entry-pathname
								    (subseq (script-name*) (length page-entry-uri-path)))))

	;;(print `(,page-entry-area-template ,page-entry-blocks))
	(when (and page-entry-area-template page-entry-area-template-key page-entry-blocks)
	  (set-html-template-value page-entry-area-template-key
				   (apply-blocks-to-area page-entry-blocks page-entry-area-template)))
	(cond (eval-args
	       (let ((eval-result (if page-entry-evaluator
				      (multiple-value-list (apply page-entry-evaluator eval-args))
				      eval-args)))
		 (apply page-entry-printer eval-result)))
	      (t (http-read-eval-print-loop "error/404")))))))

(defparameter *error-code->env-contructor*
  (list (list 404 :uri 'script-name*)))

(site-function set-env/get-error-template (pathname)
  (bind-if (error-code (get-env-value :error-code))
	   (destructuring-bind (code key fn)
	       (assoc error-code *error-code->env-contructor*)
	     (declare (ignore code))
	     (append-env-value! :html-template (list key (funcall fn)))
	     pathname)
	   (error "FIXME:Programming error?")))

(defmethod %merge-pathnames ((filename pathname) folder)
  (merge-pathnames filename folder))

(defmethod %merge-pathnames ((filename string) folder)
  (format nil "~A~A" folder filename))

(defun search-file-in-abstract5-system (filename)
  (loop for folder in (list #+FIXME *selected-theme*
			    (and (boundp '*selected-site*) (site-home-folder *selected-site*))
			    *abstract5-home*)
     as merged-pathname = (%merge-pathnames filename folder)
     when (setf folder (probe-file merged-pathname))
     do (return-from search-file-in-abstract5-system folder))
  (error "Requested file ~S not found in theme, site and global folders" filename))

(defun print-html-template (filename env)
  (with-existing-abstract5-file (pathname filename)
    (with-output-to-string (out)
      (html-template:fill-and-print-template pathname
					     env
					     :stream out))))

(defun print-standard-html-template (filename)
  (let ((html-template:*string-modifier* #'identity))
    (print-html-template filename (get-env-value :html-template))))


(site-function print-static-file (pathname)
  (if (and pathname (probe-file pathname))
      (hunchentoot::handle-static-file pathname )
      (http-read-eval-print-loop "error/404")))

(site-function uri-file->full-pathname (relative-pathname)
  (with-existing-abstract5-file (pathname relative-pathname)
    pathname))
;;
;; Dashboard implementation
;;
(defun eval-dashboard-request (pathname)
  (append-env-value! :html-template `( ;; FIXME: use concrete5's translation
				     :return-to-website ,(translate "Return to Website")
				     :help ,(translate "Help")
				     :sign-out ,(translate "Sign Out")
				     :version-string ,(translate "Version")
				     :app-version "0.1"
				     ;; FIXME: add permision check
				     :nav-list ,(loop for child in (page-entry-children (find-page-entry "/dashboard"))
						   collect (list :active (eq child *current-page-entry*)
								 :href (format nil "~A~A"
									       (page-entry-uri-path child)
									       (page-entry-uri-filename child))
								 :nav-name (page-entry-name child)
								 :nav-description (page-entry-description child)))))
  pathname)


;;
;; /help/
;;
(site-function http-request-using-uri (pathname)
  (declare (ignore pathname))
  ;; http-request returns several vlues. Use first one only
  (nth-value 0 (drakma:http-request (get-env-value :uri))))

;;
;; /dashboard block
;;
(site-function eval-dashboard-activity (filename)
  (print-html-template filename
		       `(:num-visits-string ,(translate "Number of visits since your previous login")
			 :num-visits 911
			 :total-visits-string ,(translate "Total visits")
			 :total-visits 1999
			 :total-page-versions-string ,(translate "Total page versions")
			 :total-page-versions 1984
			 :last-edit-string ,(translate "Last edit")
			 :last-edit "Yesterday"
			 :last-login-string ,(translate "Last login")
			 :last-login "Yesterday"
			 :total-edit-mode-string ,(translate "Total pages in edit mode")
			 :total-edit-mode 99
			 :total-form-string ,(translate "Total form submissions")
			 :total-form-today 1
			 :today ,(translate "today")
			 :total-form-submissions 9
			 :total ,(translate "total"))))

;;; HTML-PAGE-CORE.LISP ends here
