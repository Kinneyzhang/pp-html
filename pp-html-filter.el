;;; pp-html-filter.el --- pp-html filter function.

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/usrname/pp-html-filter
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;;; Code:

(require 's)

(defvar pp-html-filter-list
      '((:add pp-html-filter-add)
	(:abs pp-html-filter-abs)
	(:append pp-html-filter-concat)
	(:capitalize pp-html-filter-capitalize)
	(:compact pp-html-filter-compact)
	(:concat pp-html-filter-concat)
	(:default pp-html-filter-default)
	(:downcase pp-html-filter-downcase)
	(:upcase pp-html-filter-upcase)
	(:escape pp-html-filter-escape)
	(:join pp-html-filter-join))
      "Filter function tag.")

;; filter evaluation
(defun pp-html--filter-p (el)
  "Judge if a element is a filter."
  (if (listp el)
      (if (and (eq '/ (car el)))
	  t nil)
    nil))

(defun pp-html--filter-alist (plist)
  "Make filter plist to alist"
  (if (null plist)
      '()
    (if (assoc (car plist) pp-html-filter-list)
	(if (not (assoc (cadr plist) pp-html-filter-list))
	    (cons
	     (list (car plist) (cadr plist))
	     (pp-html--filter-alist (cddr plist)))
	  (cons
	   (list (car plist))
	   (pp-html--filter-alist (cdr plist)))))))

(defun pp-html--filter-eval (sexp)
  "Evalute pp-html filter."
  (let* ((value (pp-html-eval (cadr sexp)))
	 (plist (cddr sexp))
	 (alist (pp-html--filter-alist plist)))
    (dolist (filter alist)
      (if (null (cadr filter))
	  (progn
	    (setq value
		  (funcall (cadr (assoc (car filter) pp-html-filter-list)) value)))
	(setq value
	      (funcall (cadr (assoc (car filter) pp-html-filter-list)) value (cadr filter)))))
    value))

;;;###autoload
(defun pp-html-define-filter (filter func)
  "Add pp-html filter."
  (if (not (assoc filter pp-html-filter-list))
      (add-to-list 'pp-html-filter-list (list filter func))
    (error "Filter %s already defined!" filter)))

;; filter functions
(defun pp-html-filter-abs (value)
  "Return the absolute value of a number."
  (let ((value (if (stringp value)
		   (string-to-number value)
		 value)))
    (abs value)))

(defun pp-html-filter-add (value arg)
  "Add a value to a number"
  (let ((arg (if (stringp arg)
		 (string-to-number arg)
	       arg)))
    (+ value arg)))

(defun pp-html-filter-append (value arg)
  "Append a list to another one."
  (append value arg))

(defun pp-html-filter-capitalize (value)
  "Convert the first word’s first character to upper case."
  (s-capitalize value))

(defun pp-html-filter-compact (value)
  "Delete all nil in a list."
  (delete nil value))

(defun pp-html-filter-concat (value)
  "Join two string together."
  (concat value (pp-html-eval arg)))

(defun pp-html-filter-default (value arg)
  "If value is nil or null string, set default value."
  (if (or (string= "" value) (null value))
      arg
    value))

(defun pp-html-filter-downcase (value)
  "Convert all chars in string to lower case."
  (downcase value))

(defun pp-html-filter-upcase (value)
  "Convert all chars in string to upper case."
  (upcase value))

(defun pp-html-filter-escape (value)
  "Escapes a string by replacing characters with escape sequences."
  (xml-escape-string value))

(defun pp-html-filter-join (value arg)
  "Combines the items in a list into a single string using the argument as a separator"
  (let ((res (nth 0 value)))
    (dolist (item value)
      (setq res (concat res arg item)))
    res))

;; (defun pp-html-filter-lstrip (value)
;;   "Removes all whitespace (tabs, spaces, and newlines) from a string"
;;   )

;; (defun pp-html-filter-map (value arg))

(provide 'pp-html-filter)
;;; pp-html-filter.el ends here

