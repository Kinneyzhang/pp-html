;;; pp-html-eval.el --- evaluate variable and function in sexp
;;; -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 1.0.0
;; Keywords: eval
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/pp-html
;; Package-Requires: ((emacs "26.3") (dash "2.16.0))

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
(require 'dash)
(require 'pp-html-utils)
(require 'pp-html-filter)

;; pp-html evaluation
(defun pp-html--var-p (el)
  "Jude if a element is a variable."
  (if (symbolp el)
      (if (and
	   (string= "$" (pp-html--symbol-initial el))
	   (not (string= "" (pp-html--symbol-rest el))))
	  t nil)
    nil))

(defun pp-html--var-eval (el)
  "Evaluate variable in pp-html."
  (let* ((i 1)
	 (var (intern (pp-html--symbol-rest el)))
	 (var-str (symbol-name var))
	 (lst (split-string var-str "[.]"))
	 (res (eval (intern (nth 0 lst)))))
    (if (= 1 (length lst))
	(setq res (eval var))
      (while (nth i lst)
	(let ((prop (intern (concat ":" (nth i lst)))))
	  (setq res (eval `(plist-get ',res ,prop)))
	  (incf i))))
    res))

(defun pp-html--func-p (el)
  "Jude if a element is a function."
  (if (listp el)
      (if (eq '$ (car el)) t nil)
    nil))

(defun pp-html--func-param-eval (params)
  "Evaluate function params in pp-html."
  (mapcar
   (lambda (x)
     (pp-html-eval x))
   (-tree-map
    (lambda (p)
      (if (pp-html--var-p p)
	  (setq p (pp-html-eval p))
	(setq p p)))
    params)))

(defun pp-html--func-eval (el)
  "Evaluate function in pp-html."
  (let ((params (pp-html--func-param-eval (cddr el))))
    (eval `(funcall ',(cadr el) ,@params))))

;;;###autoload
(defun pp-html-eval (el)
  "General evaluation in pp-html."
  (cond
   ((pp-html--var-p el)
    (pp-html--var-eval el))
   ((pp-html--func-p el)
    (pp-html--func-eval el))
   ((pp-html--filter-p el)
    (pp-html--filter-eval el))
   (t el)))

(provide 'pp-html-eval)
;;; pp-html-eval.el ends here
