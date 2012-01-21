;;;;   -*- Mode: lisp; Package: cl-user; Syntax: Common-lisp; encoding: utf-8; -*-
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

(in-package :cl-user)

(defconstant +3rd-party-patch-version+ "$Revision$"
  "$Id$
   Report bugs to: jongwon.choi@defstruct.com")

(in-package #:clsql-sys)

;;
;; Patched to save and use result with abstract5's persistent objects cache
;; ('abstract5' prefixed)
;;

(defun build-objects (vals sclasses immediate-join-classes sels immediate-joins database refresh flatp instances)
  "Used by find-all to build objects."
  (flet ((build-object (vals vclass jclasses selects immediate-selects instance)
	   (let ((obj (when (and (not refresh) (typep vclass 'abstract5::persistent-class))
			(abstract5::get-pobj-from-cache (nth (position 'abstract5::oid selects
								       :key #'(lambda (sl)
										(slot-definition-name (car sl))))
							     vals)))))

	     (unless obj
	       (setf obj (if instance instance (make-instance (class-name vclass) :view-database database)))
	       (let* ((db-vals (butlast vals (- (list-length vals)
						(list-length selects))))
		      (join-vals (subseq vals (list-length selects)))
		      (joins (mapcar #'(lambda (c) (when c (make-instance c :view-database database)))
				     jclasses)))
		 ;; use refresh keyword here
		 (setf obj (get-slot-values-from-view obj (mapcar #'car selects) db-vals))
		 (mapc #'(lambda (jo)
			   ;; find all immediate-select slots and join-vals for this object
			   (let* ((jo-class (class-of jo))
				  (slots
				   (if (normalizedp jo-class)
				       (class-direct-slots jo-class)
				       (class-slots jo-class)))
				  (pos-list (remove-if #'null
						       (mapcar
							#'(lambda (s)
							    (position s immediate-selects
								      :key #'car
								      :test #'eq))
							slots))))
			     (get-slot-values-from-view jo
							(mapcar #'car
								(mapcar #'(lambda (pos)
									    (nth pos immediate-selects))
									pos-list))
							(mapcar #'(lambda (pos) (nth pos join-vals))
								pos-list))))
		       joins)
		 (mapc
		  #'(lambda (jc)
		      (let* ((vslots
			      (class-slots vclass))
			     (slot (find (class-name (class-of jc)) vslots
					 :key #'(lambda (slot)
						  (when (and (eq :join (view-class-slot-db-kind slot))
							     (eq (slot-definition-name slot)
								 (gethash :join-class (view-class-slot-db-info slot))))
						    (slot-definition-name slot))))))
			(when slot
			  (setf (slot-value obj (slot-definition-name slot)) jc))))
		  joins)
		 (when refresh (instance-refreshed obj))))
	     ;; Save the object
	     (abstract5::save-pobj-to-cache obj)
	     obj)))
    (let* ((objects
            (mapcar #'(lambda (sclass jclass sel immediate-join instance)
                        (prog1
                            (build-object vals sclass jclass sel immediate-join instance)
                          (setf vals (nthcdr (+ (list-length sel) (list-length immediate-join))
                                             vals))))
                    sclasses immediate-join-classes sels immediate-joins instances)))
      (if (and flatp (= (length sclasses) 1))
          (car objects)
          objects))))

;;
;; Use weak-hash for records-cache
;;
(defun (setf records-cache-results) (results targets qualifiers database)
  (unless (record-caches database)
    (setf (record-caches database)
          (make-hash-table :test #'equal :weak :value)))
  (setf (gethash (compute-records-cache-key targets qualifiers)
                 (record-caches database)) results)
  results)

;;
;; PostgreSql text type
;; FIXME: remove?
(deftype text ()
  "Postgresql varying string"
  'string)

(export 'text :clsql-sys)

(import 'text :clsql)
(import 'text :clsql-user)

(export 'text :clsql)
(export 'text :clsql-user)

;;
;; Pool (to prevent connection mess up in multi-threads env)
;;

(in-package :clsql-postgresql-socket)

(defconstant +postgres-max-connections+ 50
  "Default value of max_connections in postgresql conf is 100.")
(defvar *postgres-connection-count* 0)
(defparameter *postgres-connection-pool* ())
(defparameter *postgresql-pool-lock* (ccl:make-lock "psql-pool-lock"))

(defun get-conn-from-pool ()
  (or (ccl:with-lock-grabbed (*postgresql-pool-lock*)
	(pop *postgres-connection-pool*))
      ;; FIXME: error handling?
      (unless (ccl:with-lock-grabbed (*postgresql-pool-lock*)
		;; ignore race condition
		(> +postgres-max-connections+ +postgres-max-connections+))
	(destructuring-bind (host database user password port)
	    (connection-spec *default-database*)
	  (prog1
	      (open-postgresql-connection :host host :port port
					  :database database :user user
					  :password password)
	    (ccl:with-lock-grabbed (*postgresql-pool-lock*)
	      (incf *postgres-connection-count*)))))))

(defun push-conn-to-pool (conn)
  (ccl:with-lock-grabbed (*postgresql-pool-lock*)
    (push conn *postgres-connection-pool*)))

(defmethod initialize-instance :after ((postgresql-socket-database postgresql-socket-database) &key)
  (push (slot-value postgresql-socket-database 'connection) *postgres-connection-pool*)
  (slot-makunbound postgresql-socket-database 'connection))

(defmethod database-disconnect ((database postgresql-socket-database))
  (dolist (conn *postgres-connection-pool*)
    (close-postgresql-connection conn))
  t)

(defvar *current-db-connection*)

(defmethod database-connection ((database postgresql-socket-database))
  *current-db-connection*)

(defmethod (setf database-connection) (new-value (database postgresql-socket-database))
  (declare (ignore new-value))
  (error "This is not an expected call in pool environment!"))

(defmacro with-connection-from-pool (&body body)
  `(progn
     (assert (not (boundp '*current-db-connection*)))
     (let ((*current-db-connection* (get-conn-from-pool)))
       (unwind-protect (progn ,@body)
	 ;; FIXME: remove unrecoverable error
	 (push-conn-to-pool *current-db-connection*)))))

;;
;; FIXME: Make Hunchentoot workers reusable
;; DB connection and HTTP connection is 1:1
;;

(in-package :html-template)

(defun create-template-printer-aux (string-stack end-token)
  "Reads from *STANDARD-INPUT* and returns a template printer from
what it reads.  When this function is entered the stream pointer must
not be inside a template tag.  STRING-STACK is a list of strings (in
reverse order) read so far which haven't been used to build a template
printer.  END-TOKEN is either NIL or one of :LOOP, :REPEAT, :IF,
:IF-ELSE, or :UNLESS-ELSE denoting that we expect certain tags to
close open TMPL_LOOP, TMPL_REPEAT, TMPL_IF, or TMPL_UNLESS tags.  This
function returns a second value which is true if, after reading
TMPL_IF or TMPL_UNLESS, a corresponding TMPL_ELSE was seen."
  (let* ((string
           ;; read text up to the next template start marker
           (read-until *template-start-marker*
                       ;; don't skip it, return it
                       :skip nil
                       :eof-action (lambda (collector)
                                     (when end-token
                                       ;; make sure we don't accept
                                       ;; EOF if there are still tags
                                       ;; waiting to be closed
                                       (signal-template-syntax-error
                                        "Unexpected EOF, ~A tag is missing"
                                        (case end-token
                                          ((:loop) "<!-- /TMPL_LOOP -->")
                                          ((:repeat) "<!-- /TMPL_REPEAT -->")
                                          ((:if :if-else) "<!-- /TMPL_IF -->")
                                          ((:unless :unless-else) "<!-- /TMPL_UNLESS -->"))))
                                     ;; otherwise (EOF before another
                                     ;; start marker was seen) just
                                     ;; return a template printer
                                     ;; which unconditionally prints
                                     ;; the rest of the stream
                                     (return-from create-template-printer-aux
                                       (create-simple-printer
                                        (cons collector string-stack))))))
         (whitespace
           ;; skip whitespace but keep it in case this turns out not
           ;; to be a template tag
           (skip-whitespace :skip nil))
         (token
           ;; read what could be a template token's name
           (with-syntax-error-location ()
             (read-while (lambda (c)
                           (or (alpha-char-p c)
                               (char= c #\_)
                               (char= c #\/)))
                         :skip nil
                         :eof-action (lambda (collector)
                                       (declare (ignore collector))
                                       ;; complain about tags which
                                       ;; haven't been closed
                                       (signal-template-syntax-error
                                        "EOF while inside of tag starting with ~S"
                                        *template-start-marker*))))))
    (cond ((string-equal token "TMPL_INCLUDE")
            ;; TMPL_INCLUDE tag - first read the pathname which has to
            ;; follow and merge it with *DEFAULT-TEMPLATE-PATHNAME*
            (let* ((pathname (read-tag-rest :read-attribute t :intern nil))
		   ;; NOTE: Custom patch
                   (merged-pathname (or (probe-file pathname)
					(merge-pathnames pathname
							 *default-template-pathname*))))
              (when (member merged-pathname *included-files*
                            :test #'equal)
                ;; raise an error if this file has been included
                ;; before - infinite recursion ahead!
                (with-syntax-error-location ()
                  (signal-template-syntax-error
                   "Infinite recursion - file ~S includes itself"
                   merged-pathname)))
              ;; otherwise create (and cache) a template printer
              (create-template-printer merged-pathname)
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
                ;; then we combine it with the strings before the tag
                ;; to create a template printer for TMPL_INCLUDE
                (values
                 (create-include-printer (cons (skip-leading-whitespace string)
                                               string-stack)
                                         merged-pathname
                                         next-fn)
                 else-follows))))
          ((string-equal token "TMPL_VAR")
            ;; TMPL_VAR tag - first read the symbol which has to
            ;; follow and intern it
            (let ((symbol (read-tag-rest :read-attribute t)))
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux nil end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; to create a template printer for TMPL_VAR - note
                 ;; that we don't skip leading and trailing whitespace
                 ;; here
                 (create-var-printer (cons string string-stack)
                                        symbol
                                        next-fn)
                 else-follows))))
          ((or (string-equal token "TMPL_LOOP")
               (string-equal token "TMPL_REPEAT"))
            ;; TMPL_LOOP or TMPL_REPEAT tag - first read the symbol
            ;; which has to follow and intern it
            (let* ((kind (if (string-equal token "TMPL_LOOP") :loop :repeat))
                   (symbol (read-tag-rest :read-attribute t))
                   ;; then read the stream up to the corresponding
                   ;; end tag and create a template printer for the
                   ;; loop body
                   (body-fn (with-syntax-error-location ()
                              (create-template-printer-aux
                               (skip-trailing-whitespace)
                               ;; this argument denotes that we expect
                               ;; to see /TMPL_LOOP or /TMPL_REPEAT and
                               ;; want to stop there
                               kind))))
              (multiple-value-bind (next-fn else-follows)
                  ;; now we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; and the body printer to create a template printer
                 ;; for TMPL_LOOP
                 (funcall (case kind
                            (:loop #'create-loop-printer)
                            (:repeat #'create-repeat-printer))
                          (cons (skip-leading-whitespace string)
                                string-stack)
                          symbol
                          body-fn
                          next-fn)
                 else-follows))))
	  ((string-equal token "TMPL_CALL")
            ;; TMPL_CALL tag - first read the symbol which has to
            ;; follow and intern it
           (let ((symbol (read-tag-rest :read-attribute t)))
             (multiple-value-bind (next-fn else-follows)
                  ;; recursively create the template printer for the
                  ;; rest of the stream
                 (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
               ;; create the printer that will output the strings
               ;; before this tag and call the templates stored under
               ;; SYMBOL
               (values (funcall #'create-call-printer
                                (cons (skip-leading-whitespace string)
                                      string-stack)
                                symbol
                                next-fn)
                       else-follows))))
          ((string-equal token "/TMPL_LOOP")
            (unless (eq end-token :loop)
              ;; check if we expected /TMPL_LOOP here, i.e. if an open
              ;; TMPL_LOOP was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_LOOP")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_LOOP body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_REPEAT")
            (unless (eq end-token :repeat)
              ;; check if we expected /TMPL_REPEAT here, i.e. if an open
              ;; TMPL_REPEAT was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_REPEAT")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_REPEAT body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((or (string-equal token "TMPL_IF")
               (string-equal token "TMPL_UNLESS"))
            ;; TMPL_IF or TMPL_UNLESS tag - first read the symbol
            ;; which has to follow and intern it
            (let ((symbol (read-tag-rest :read-attribute t))
                  (unlessp (string-equal token "TMPL_UNLESS")))
              (multiple-value-bind (if-fn else-follows)
                  (with-syntax-error-location ()
                    ;; then read the stream up to the corresponding
                    ;; TMPL_ELSE, /TMPL_IF, or /TMPL_UNLESS and create
                    ;; a template printer for the "if" (or "unless") part
                    (create-template-printer-aux
                     (skip-trailing-whitespace)
                     ;; this argument denotes that we expect to see
                     ;; TMPL_ELSE _or_ one of /TMPL_IF, /TMPL_UNLESS and,
                     ;; in the second case, want to stop there
                     (if unlessp :unless-else :if-else)))
                (let ((else-fn (if else-follows
                                 ;; if we encountered TMPL_ELSE read
                                 ;; the stream up to the corresponding
                                 ;; /TMPL_IF or /TMPL_UNLESS and
                                 ;; create a template printer for the "else" part
                                 (with-syntax-error-location ()
                                   (create-template-printer-aux
                                    (skip-trailing-whitespace)
                                    ;; this argument denotes that we
                                    ;; expect to see /TMPL_IF or /TMPL_UNLESS
                                    ;; (but not TMPL_ELSE) and want to stop
                                    ;; there
                                    (if unlessp :unless :if)))
                                 ;; use a dummy printer for the "else"
                                 ;; part if we didn't see TMPL_ELSE
                                 #'no-values)))
                  (multiple-value-bind (next-fn else-follows)
                      ;; now we recursively create the template printer
                      ;; for the rest of the stream
                      (create-template-printer-aux (skip-trailing-whitespace)
                                                   end-token)
                    (values
                     ;; then we combine it with the strings before the
                     ;; tag and the "if" and "else" parts to create a
                     ;; template printer for TMPL_IF or TMPL_UNLESS
                     (create-if-printer (cons (skip-leading-whitespace string)
                                              string-stack)
                                        symbol
                                        if-fn
                                        else-fn
                                        next-fn
                                        unlessp)
                     else-follows))))))
          ((string-equal token "TMPL_ELSE")
            (unless (member end-token '(:if-else :unless-else))
              ;; check if we expected /TMPL_ELSE here, i.e. if an open
              ;; TMPL_IF or TMPL_UNLESS was pending and we haven't
              ;; seen TMPL_ELSE before
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected TMPL_ELSE")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" part
            (values
             (create-simple-printer (cons (skip-leading-whitespace string)
                                          string-stack))
             ;; return a true second value to denote that we've seen
             ;; TMPL_ELSE
             t))
          ((string-equal token "/TMPL_IF")
            (unless (or (eq end-token :if) (eq end-token :if-else))
              ;; check if we expected /TMPL_IF here, i.e. if an open
              ;; TMPL_IF was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_IF")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_UNLESS")
            (unless (or (eq end-token :unless) (eq end-token :unless-else))
              ;; check if we expected /TMPL_UNLESS here, i.e. if an open
              ;; TMPL_UNLESS was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_UNLESS")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "unless" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          (t
            ;; we couldn't identify a valid tag, so we treat
            ;; everything we've read so far as a literal string and
            ;; carry on - if we're lucky our CL implementation will
            ;; optimize this tail call into an iterative loop
            (create-template-printer-aux
             (cons token
                   (cons whitespace
                         (cons *template-start-marker*
                               (cons string string-stack))))
             end-token)))))

(defmethod fill-and-print-template ((pathname pathname) values
                                    &rest rest
                                    &key (stream *default-template-output*))
  (remf rest :stream)
  (let ((*template-output* stream)
	;; NOTE: Custom patch - set *default-template-output*
	;; to current directory. When TMPL_INCLUDE has only filename, it will try the same directory
	;; as the current template file.
	(*default-template-pathname* (make-pathname :directory (pathname-directory pathname))))
    (funcall (apply #'create-template-printer pathname rest) values)))

;;; 3RD-PARTY-PATCH.LISP ends here
