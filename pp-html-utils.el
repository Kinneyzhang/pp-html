;;; pp-html-utils.el --- some useful functions.
;;; -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 1.0.0
;; Keywords: util
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/pp-html
;; Package-Requires: ((emacs "26.3"))

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

(defun pp-html--plist->alist (plist)
  "Convert plist to alist."
  (if (null plist)
      '()
    (cons
     (list (car plist) (cadr plist))
     (pp-html--plist->alist (cddr plist)))))

(defun pp-html--symbol-initial (sym)
  "Get the initial charactor of a symbol."
  (if (symbolp sym)
      (substring (symbol-name sym) 0 1)))

(defun pp-html--symbol-rest (sym)
  "Get the rest part of symbol."
  (if (symbolp sym)
      (substring (symbol-name sym) 1)))

(provide 'pp-html-utils)
;;; pp-html-utils.el ends here

