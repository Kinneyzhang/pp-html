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

(defvar pp-html-filter-list
  '(:add :date)
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
    (if (member (car plist) pp-html-filter-list)
	(if (not (member (cadr plist) pp-html-filter-list))
	    (cons
	     (list (car plist) (cadr plist))
	     (pp-html--filter-alist (cddr plist)))
	  (cons
	   (list (car plist))
	   (pp-html--filter-alist (cdr plist)))))))

(defun pp-html--filter-eval (el)
  "Evalute pp-html filter."
  (let* ((target (pp-html--eval (cadr el)))
	 (plist (cddr el))
	 (alist (pp-html--filter-alist plist)))
    (dolist (filter alist)
      (if (null (cadr filter))
	  (setq target
		(funcall (read (concat "pp-html-filter-" (pp-html--symbol-rest (car filter)))) target))
	(setq target
	      (funcall (read (concat "pp-html-filter-" (pp-html--symbol-rest (car filter)))) (cadr filter) target))))
    target))


;; Add filter macro.
(defmacro pp-html-add-filter (name func)
  "Add pp-html filter."
  (if (member name pp-html-filter-list)
      (add-to-list pp-html-filter-list name)
    pp-html-filter-list))

;; filter functions
(defun pp-html-filter-add (param target)
  (let ((param (if (stringp param)
		   (string-to-number param)
		 param)))
    (+ param target)))

(provide 'pp-html-filter)
;;; pp-html-filter.el ends here

