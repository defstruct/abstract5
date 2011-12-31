;;;;   -*- Mode: lisp; Package: utils; Syntax: Common-lisp -*-
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

(defconstant +utils-version+ "$Revision: 278419e59d12 $"
  "$Id: utils.lisp,v 278419e59d12 2011/08/13 21:04:44 Jong-won Choi $
   Report bugs to: jongwon.choi@defstruct.com")

(defun obfuscate-for-js (string)
  (with-output-to-string (out)
    (loop for char across string
       and i from 0

       if (evenp i)
       do (format out "&#~D;" (char-code char))
       else
       do (princ char out))))

(defvar *split-sequence* nil)

(defun split-sequence (delimiter seq &rest args &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  (declare (ignore count remove-empty-subseqs from-end start end test test-supplied test-not test-not-supplied key key-supplied))
  (apply (or *split-sequence*
	     (setf *split-sequence* (find-symbol "SPLIT-SEQUENCE" 'usocket)))
	 delimiter seq args))

(defmacro bind-when ((var exp) &body body)
  `(let ((,var ,exp))
     (when ,var
       ,@body)))

(defmacro bind-if ((var exp) if-body else-body)
  `(let ((,var ,exp))
     (if ,var
	 ,if-body
	 ,else-body)))

(defmacro bind-case ((var exp) &body body)
  `(let ((,var ,exp))
     (case ,var
       ,@body)))

(defun empty-string-p (str)
  (zerop (length str)))
#+XXX
(defun read-text-file (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((sequence (make-array (file-length in)
				:element-type '(unsigned-byte 8))))
      (read-sequence sequence in)
      (ccl:decode-string-from-octets sequence :external-format :utf-8))))

(defun make-circular-list (list)
  (setf (cdr (last list)) list))

(defun curry (function &rest args)
  "Curry the ARGS to FUNCTION, placing ARGS first."
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(defun curry-right (function &rest args)
  "Curry the ARGS to FUNCTION, placing ARGS last."
  (lambda (&rest more-args)
    (apply function (append more-args args))))

(defconstant +whitespace-characters+
  (list #\Tab #\Space #\Page #\Return #\Newline #\Linefeed))

(defun whitespace-char? (x) (member x +whitespace-characters+ :test #'char=))

(defun whitespace-string? (str)
  (every #'whitespace-char? str))

(defun string-trim-whitespace (str)
  (when str
    (string-trim +whitespace-characters+ str)))

(defun split-path&name (pathname)
  (assert (char= (aref pathname 0) #\/))
  (let ((end/ (position #\/ pathname :from-end t :test #'char=)))
    `(,(subseq pathname 0 (incf end/)) ,(when (> (length pathname) end/)
					      (subseq pathname end/)))))


;;; UTILS.LISP ends here
