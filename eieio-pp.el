;;; eieio-pp.el --- prettier pretty-printing for eieio

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120118
;; Version: 0.1.0
;; Homepage: https://github.com/tarsius/eieio-pp
;; Keywords: OO, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prettier pretty-printing for EIEIO.

;; This library REDEFINES functions DEFINED IN `eieio.el':
;;
;; - `object-write'(eieio-default-superclass)
;; - `eieio-override-prin1'
;; - `eieio-list-prin1'
;;
;; The new versions only differ in that they print less whitespace
;; and don't quote keywords and nil.

;;; Code:

(require 'eieio)

(defmethod object-write ((this eieio-default-superclass) &optional comment)
  "Write object THIS out to the current stream.
This writes out the vector version of this object.  Complex and recursive
object are discouraged from being written.
  If optional COMMENT is non-nil, include comments when outputting
this object."
  (when comment
    (princ ";; Object ")
    (princ (object-name-string this))
    (princ "\n")
    (princ comment)
    (princ "\n"))
  (let* ((cl (object-class this))
	 (cv (class-v cl)))
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ "(")
    (princ (symbol-name (class-constructor (object-class this))))
    (princ " \"")
    (princ (object-name-string this))
    (princ "\"")
    (let ((publa (aref cv class-public-a))
	  (publd (aref cv class-public-d))
	  (publp (aref cv class-public-printer))
	  (eieio-print-depth (1+ eieio-print-depth)))
      (while publa
	(when (slot-boundp this (car publa))
	  (let ((i (class-slot-initarg cl (car publa)))
		(v (eieio-oref this (car publa))))
	    (unless (or (not i) (equal v (car publd)))
	      (unless (looking-back "\n")
		(princ "\n"))
	      (princ (make-string (* eieio-print-depth 2) ? ))
	      (princ (symbol-name i))
	      (if (car publp)
		  (funcall (car publp) v)
		(eieio-override-prin1 v)))))
	(setq publa (cdr publa) publd (cdr publd)
	      publp (cdr publp))))
    (princ ")")
    (when (= eieio-print-depth 0)
      (princ "\n"))))

(defun eieio-override-prin1 (thing)
  "Perform a `prin1' on THING taking advantage of object knowledge."
  (cond ((eieio-object-p thing)
	 (princ "\n")
	 (object-write thing))
	((listp thing)
	 (princ " ")
	 (eieio-list-prin1 thing))
	((class-p thing)
	 (princ " ")
	 (princ (class-name thing)))
	((or (keywordp thing) (not thing))
	 (princ " ")
	 (prin1 thing))
	((symbolp thing)
	 (princ " '")
	 (princ (symbol-name thing)))
	(t
	 (princ " ")
	 (prin1 thing))))

(defun eieio-list-prin1 (list)
  "Display LIST where list may contain objects."
  (if (not (eieio-object-p (car list)))
      (progn
	(princ "'")
	(prin1 list))
    (princ "(list")
    (when (eieio-object-p (car list))
      (princ "\n"))
    (let ((eieio-print-depth (1+ eieio-print-depth))
	  thing)
      (while list
	(setq thing (car list))
	(if (eieio-object-p thing)
	    (object-write thing)
	  (unless (or (keywordp thing) (not thing))
	    (princ "'"))
	  (prin1 thing))
	(setq list (cdr list))))
    (princ ")")))

(provide 'eieio-pp)
;;; eieio-pp.el ends here
